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
  # El maestro de nginx corre como root pero los TRABAJADORES corren como
  # www-data, y son ellos los que leen este archivo en cada petición. Con 600
  # y dueño root no pueden abrirlo, y nginx contesta 500 a todo: ni 401 ni
  # 502, un 500 seco que no dice por qué. De ahí el grupo www-data y el 640.
  # Lo que guarda es un hash, no la contraseña.
  chown root:www-data /etc/nginx/.htpasswd
  chmod 640 /etc/nginx/.htpasswd
  AUTH_BLOCK="auth_basic \"shiny_a1\"; auth_basic_user_file /etc/nginx/.htpasswd;"
  echo "[entrypoint] autenticación activada para el usuario '${user}'"
else
  echo "[entrypoint] sin APP_PASSWORD: el sitio queda abierto"
fi

# Por omisión nginx escribe sus errores a /var/log/nginx/error.log, o sea a un
# archivo dentro del contenedor que nadie va a leer nunca. Railway solo muestra
# stdout y stderr: sin esto, un fallo de configuración se ve como un 500 en
# blanco y no hay forma de saber de qué se queja. Nivel `warn` y no `info`
# porque en `info` cada cierre de conexión escribe un renglón y el log real se
# ahoga; lo que importa para diagnosticar (un permiso denegado al leer las
# contraseñas, un upstream caído) se registra en `error` o `crit`.
sed -i 's|^\s*error_log .*|error_log /dev/stderr warn;|' /etc/nginx/nginx.conf

# La portada enlaza al análisis destacado; la URL se arma aquí porque el HTML
# es estático y los términos vienen del entorno.
if [[ -n "${FEATURED_KW:-}" ]]; then
  featured="forecasting/?kw=$(printf '%s' "$FEATURED_KW" | sed 's/ /%20/g; s/,/%2C/g')&geo=${FEATURED_GEO:-MX}&time=$(printf '%s' "${FEATURED_TIME:-today+5-y}" | sed 's/+/%2B/g')"
  sed -i "s|__FEATURED_URL__|${featured}|g; s|__FEATURED_KW__|${FEATURED_KW}|g" /srv/shiny-server/index.html
else
  sed -i "s|__FEATURED_URL__|forecasting/|g; s|__FEATURED_KW__|un ejemplo|g" /srv/shiny-server/index.html
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

  access_log /dev/stdout;
  error_log  /dev/stderr warn;

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

    # Shiny Server manda X-Frame-Options: DENY. Se quita y se sustituye por la
    # lista de quién puede enmarcar estas apps: el propio sitio. Así abelhga.com
    # puede meterlas en un iframe el día que quiera, y nadie más.
    proxy_hide_header X-Frame-Options;
    add_header Content-Security-Policy "frame-ancestors 'self' https://www.abelhga.com https://abelhga.com" always;
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
SHINY_PID=$!

nginx -g 'daemon off;' &
NGINX_PID=$!

# Comprobación de arranque contra uno mismo.
#
# Está aquí porque desde fuera no siempre se puede llegar: quien despliega
# puede tener bloqueado el dominio, y entonces la única evidencia de que esto
# sirve o no son estos logs. Dos peticiones bastan para distinguir los
# desenlaces que importan — 401 y 200 es que todo está bien; 500 es que nginx
# no puede leer el archivo de contraseñas; 502 es que Shiny Server no levantó.
#
# `set +e` porque es un diagnóstico: que falle una consulta no puede tumbar el
# contenedor. Y `disown` para que el `wait` de abajo no la cuente como uno de
# los procesos que vigila.
autocomprobacion() {
  set +e
  local sin_clave con_clave codigo ruta
  local cred=()
  [[ -n "$APP_PASSWORD" ]] && cred=(-u "${APP_USER:-abel}:${APP_PASSWORD}")

  # Esperar a que nginx conteste algo, sea lo que sea.
  for _ in $(seq 1 60); do
    sleep 1
    curl -s -o /dev/null "http://127.0.0.1:${PORT}/" && break
  done

  sin_clave="$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:${PORT}/")"
  if [[ -n "$APP_PASSWORD" ]]; then
    con_clave="$(curl -s -o /dev/null -w '%{http_code}' "${cred[@]}" "http://127.0.0.1:${PORT}/")"
    echo "[autocomprobación] portada sin credenciales: ${sin_clave} (se espera 401) · con credenciales: ${con_clave} (se espera 200)"
  else
    echo "[autocomprobación] portada: ${sin_clave} (se espera 200; el sitio está abierto)"
  fi

  for ruta in forecasting network amazon wiki; do
    codigo="$(curl -s -o /dev/null -w '%{http_code}' "${cred[@]}" "http://127.0.0.1:${PORT}/${ruta}/")"
    echo "[autocomprobación] /${ruta}/ -> ${codigo} (se espera 200)"
  done

  # El enlace destacado (el del post): pedir su HTML arranca el proceso de R
  # del forecast, y ese proceso precalienta la caché de Trends al iniciar.
  if [[ -n "${FEATURED_KW:-}" ]]; then
    codigo="$(curl -s -o /dev/null -w '%{http_code}' "${cred[@]}" "http://127.0.0.1:${PORT}/forecasting/?kw=$(printf '%s' "$FEATURED_KW" | sed 's/ /%20/g')")"
    echo "[autocomprobación] enlace destacado -> ${codigo} (el precalentado se ve en el log de la app)"
  fi

  # ¿Viaja el embudo en el HTML? El marcador lo pone gate_head() en <head>.
  html="$(curl -s "${cred[@]}" "http://127.0.0.1:${PORT}/forecasting/")"
  if printf '%s' "$html" | grep -q 'name="gate-app"'; then
    echo "[autocomprobación] gate: presente en /forecasting/ (GATE_ENABLED=${GATE_ENABLED:-no})"
  else
    echo "[autocomprobación] gate: AUSENTE en /forecasting/ — diagnóstico:"
    echo "[autocomprobación]   bytes=${#html} title=$(printf '%s' "$html" | grep -o '<title>[^<]*' | head -1)"
    printf '%s' "$html" | grep -o '<meta[^>]*>' | head -8 | sed 's/^/[autocomprobación]   /'
    printf '%s' "$html" | head -c 400 | tr '\n' ' ' | sed 's/^/[autocomprobación]   inicio: /'; echo
    echo "[autocomprobación]   logs de app: $(ls /var/log/shiny-server 2>/dev/null | tr '\n' ' ')"
    for f in /var/log/shiny-server/forecasting-*.log; do
      [[ -f "$f" ]] && tail -n 15 "$f" | sed "s|^|[autocomprobación]   $(basename "$f"): |"
    done
    echo "[autocomprobación]   xtail=$(command -v xtail || echo no) shiny-server.sh=$([[ -x /usr/bin/shiny-server.sh ]] && echo sí || echo no)"
  fi
}
autocomprobacion &
disown

# Si cualquiera de los DOS procesos que importan se muere, que se muera el
# contenedor: un nginx vivo sirviendo 502 sobre un Shiny caído parece
# "desplegado" y no lo está. Los PID van explícitos porque `wait -n` a secas
# también daría por terminada la autocomprobación, que acaba en segundos.
wait -n "$SHINY_PID" "$NGINX_PID"
exit $?
