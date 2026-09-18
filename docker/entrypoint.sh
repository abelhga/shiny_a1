#!/bin/bash
# ---------------------------------------------------------------------------
# Arranque del contenedor: nginx delante, Shiny Server detrás.
#
# Resuelve dos cosas que Shiny Server abierto no hace por sí solo:
#
#   1. El puerto. Railway (y casi cualquier PaaS) inyecta $PORT en tiempo de
#      ejecución, pero el `listen` de shiny-server.conf es un número fijo en
#      un archivo. nginx escucha en $PORT y reenvía al 3838 de dentro.
#
#   2. La autenticación. La edición abierta de Shiny Server no tiene ninguna;
#      es una función de Connect. Con APP_USER y APP_PASSWORD puestas, nginx
#      pide usuario y contraseña. SIN APP_PASSWORD el sitio queda abierto, que
#      es lo correcto para un despliegue público: así una sola imagen sirve
#      para la instancia privada y para la pública, y lo único que cambia es
#      el entorno.
#
# La contraseña nunca vive en el repositorio ni en la imagen: llega por
# variable y se hashea aquí, en cada arranque.
# ---------------------------------------------------------------------------
set -euo pipefail

PORT="${PORT:-8080}"
APP_USER="${APP_USER:-}"
APP_PASSWORD="${APP_PASSWORD:-}"

AUTH_BLOCK=""
if [[ -n "$APP_PASSWORD" ]]; then
  user="${APP_USER:-abel}"
  # apr1 es el formato que entiende nginx y que openssl sabe generar, así que
  # no hace falta traer apache2-utils solo por htpasswd.
  hash="$(openssl passwd -apr1 "$APP_PASSWORD")"
  printf '%s:%s\n' "$user" "$hash" > /etc/nginx/.htpasswd
  chmod 600 /etc/nginx/.htpasswd
  AUTH_BLOCK="auth_basic \"shiny_a1\"; auth_basic_user_file /etc/nginx/.htpasswd;"
  echo "[entrypoint] autenticación activada para el usuario '${user}'"
else
  echo "[entrypoint] sin APP_PASSWORD: el sitio queda abierto"
fi

cat > /etc/nginx/sites-available/default <<NGINX
# Generado en el arranque por entrypoint.sh. Editarlo aquí no sirve de nada:
# se reescribe en cada despliegue.

# Shiny habla por websocket. Sin este map, la conexión se cae en cuanto la app
# carga y el navegador enseña "Disconnected from the server" — el síntoma
# clásico de poner un proxy delante de Shiny sin pensarlo.
map \$http_upgrade \$connection_upgrade {
  default upgrade;
  ''      close;
}

server {
  listen ${PORT};
  server_name _;

  ${AUTH_BLOCK}

  # Una cosecha de cientos de consultas tarda; el navegador no debe darse por
  # vencido mientras la app sigue trabajando.
  proxy_read_timeout  3600s;
  proxy_send_timeout  3600s;

  location / {
    proxy_pass http://127.0.0.1:3838;
    proxy_http_version 1.1;
    proxy_set_header Upgrade    \$http_upgrade;
    proxy_set_header Connection \$connection_upgrade;
    proxy_set_header Host       \$host;
    proxy_set_header X-Real-IP  \$remote_addr;
    proxy_set_header X-Forwarded-For   \$proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto \$scheme;

    # Sin esto, nginx acumula la respuesta y la barra de progreso de una
    # cosecha larga llega toda de golpe al final.
    proxy_buffering off;
  }
}
NGINX

nginx -t

# rocker/shiny trae este script: crea /var/log/shiny-server y, con xtail,
# vuelca los logs de cada app a stdout — que es lo único que Railway lee. Sin
# él, un error de R se queda escrito dentro del contenedor, invisible.
if [[ -x /usr/bin/shiny-server.sh ]]; then
  /usr/bin/shiny-server.sh &
else
  mkdir -p /var/log/shiny-server
  chown shiny:shiny /var/log/shiny-server
  shiny-server &
fi

nginx -g 'daemon off;' &

# Si cualquiera de los dos se muere, que se muera el contenedor: un nginx vivo
# sirviendo 502 sobre un Shiny caído parece "desplegado" y no lo está.
wait -n
exit $?
