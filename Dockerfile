# ---------------------------------------------------------------------------
# Las cuatro apps de este repositorio en un contenedor, servidas por Shiny
# Server bajo un solo puerto, detrás de una capa de autenticación opcional.
#
# La base es rocker/shiny en la MISMA versión de R que declaran los
# manifest.json (4.3.3). No es cosmético: esa imagen trae Posit Package
# Manager configurado como repositorio por omisión, así que los paquetes
# llegan como BINARIOS ya compilados para esta distribución. Con el CRAN de
# siempre, prophet arrastra rstan y la construcción se va a cuarenta minutos
# de compilación de C++; con binarios son un par de minutos.
#
# Construir y correr en local:
#   docker build -t shiny-a1 .
#   docker run --rm -p 8080:8080 -e PORT=8080 shiny-a1
#   # con contraseña:
#   docker run --rm -p 8080:8080 -e PORT=8080 \
#     -e APP_USER=abel -e APP_PASSWORD=secreto shiny-a1
# ---------------------------------------------------------------------------
FROM rocker/shiny:4.3.3

# Librerías de sistema que los paquetes de R enlazan. Cada una está aquí por
# un paquete concreto: sin ellas, install2.r falla al compilar o el binario
# no carga en tiempo de ejecución.
#   libglpk40      igraph (los algoritmos de flujo)
#   libxml2        igraph y tm, al leer XML
#   libcurl/libssl httr y curl, que son cómo hablan las cuatro apps
#   nginx-light    el proxy de enfrente (puerto y autenticación)
#   openssl        genera el hash de la contraseña en el arranque
RUN apt-get update && apt-get install -y --no-install-recommends \
      libglpk40 \
      libxml2 \
      libcurl4-openssl-dev \
      libssl-dev \
      nginx-light \
      openssl \
    && rm -rf /var/lib/apt/lists/*

# Los paquetes, en su propia capa y antes de copiar las apps: así cambiar un
# app.R no vuelve a instalar nada. Solo van los de primer nivel — los otros
# noventa del manifest son dependencias y R las resuelve sola.
#   -e   que un fallo tumbe la construcción en vez de dejar la imagen a medias
#   -n 4 cuatro instalaciones en paralelo
RUN install2.r -e -n 4 \
      shiny bslib dplyr httr jsonlite DT plotly shinycssloaders \
      igraph visNetwork \
      gtrendsR prophet lubridate countrycode \
      tm stringi \
    && rm -rf /tmp/downloaded_packages

# La configuración de Shiny Server y el arranque.
COPY docker/shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY docker/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Las apps. Cada carpeta ya trae su copia de shared/ en R/ (tools/sync_shared.sh),
# y cada app.R hace source("R/...") con ruta relativa: Shiny Server pone el
# directorio de trabajo en la carpeta de la app, así que se copian tal cual.
# Las rutas cortas son las que acaban en la URL.
COPY docker/index.html            /srv/shiny-server/index.html
COPY Forecasting-trends/          /srv/shiny-server/forecasting/
COPY network/                     /srv/shiny-server/network/
COPY AmazonNetwork/               /srv/shiny-server/amazon/
COPY WikiNetwork/                 /srv/shiny-server/wiki/

# Railway inyecta PORT; 8080 es solo el valor por omisión para correr en local.
ENV PORT=8080
EXPOSE 8080

CMD ["/usr/local/bin/entrypoint.sh"]
