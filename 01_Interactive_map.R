library(leaflet)
library(dplyr)
library(htmlwidgets)

# Buscar el archivo gps_data_ más reciente
gps_files <- list.files(pattern = "gps_data_\\d+.*\\.csv")
latest_file <- gps_files[which.max(file.info(gps_files)$mtime)]

# Leer el archivo más reciente
gps.df <- read.csv(latest_file)

# Convertir la columna de fecha a formato adecuado
gps.df$datetimeGMT <- as.POSIXct(gps.df$datetimeGMT, format="%Y-%m-%d %H:%M:%S", tz="GMT")

# Filtrar datos de los últimos 7 días
lst_days <- as.POSIXct(Sys.time(), tz="GMT") - (7 * 24 * 60 * 60)
gps.df2 <- gps.df %>% filter(datetimeGMT >= lst_days)

# Paleta de colores: una por birdID, sin límite de 9 (evita el aviso de Set1)
ids <- sort(unique(gps.df$birdID))
pal.colors <- colorFactor(palette = scales::hue_pal()(length(ids)), domain = ids)

# Crear el mapa base
leafletOptions <- leaflet::leafletOptions(preferCanvas = TRUE)
imap <- leaflet(options = leafletOptions) %>%
  addTiles(options = tileOptions(maxZoom = 10))

# Agregar capas de tracks por cada individuo
grupos <- unique(gps.df$birdID)

for (bird in grupos) {
  gps.ind <- gps.df2 %>% filter(birdID == bird)
  
  # Saltar si gps.ind está vacío o todo son NA
  if (nrow(gps.ind) == 0 || all(is.na(gps.ind$datetimeGMT))) {
    cat("Advertencia: No hay datos válidos para", bird, "\n")
    next
  }
  
  lst_pos <- gps.ind %>% filter(datetimeGMT == max(datetimeGMT))  # Último punto
  
  imap <- imap %>%
    addPolylines(
      lng = ~longitude, lat = ~latitude,
      data = gps.ind,
      color = 'darkgrey',
      weight = 0.75, opacity = 0.7,
      group = bird
    ) %>%
    addCircleMarkers(
      lng = ~longitude, lat = ~latitude,
      data = gps.ind,
      radius = 0.5, color = ~pal.colors(birdID),
      popup = ~paste("ID:", birdID, "<br>Fecha:", datetimeGMT, "Sex:", sex),
      group = bird
    ) %>%
    addAwesomeMarkers(
      lng = ~longitude, lat = ~latitude,
      data = lst_pos,
      icon = awesomeIcons(
        icon = 'star', library = 'fa', markerColor = 'red'
      ),
      popup = ~paste("ID:", birdID, "<br>Fecha:", datetimeGMT, "Sex:", sex),
      group = bird
    )
}

# Marca de tiempo
timestamp <- format(Sys.time(), "%Y%m%d_%H%M", tz = "GMT", usetz = TRUE)
update_label <- paste0("Última actualización: ", format(Sys.time(), "%d-%m-%Y %H:%M", tz = "GMT", usetz = TRUE))

imap <- imap %>%
  addControl(html = update_label, position = "topright")

if (length(imap$x$calls) == 0) {
  imap <- imap %>% addControl(html = "Sin datos recientes para mostrar", position = "topright")
}

# Limpiar salida anterior y asegurar que existe docs/
if (file.exists("docs/index.html")) file.remove("docs/index.html")
if (dir.exists("docs/index_files")) unlink("docs/index_files", recursive = TRUE)
dir.create("docs", showWarnings = FALSE, recursive = TRUE)

# Guardar el mapa en docs/ con sus dependencias en docs/index_files
saveWidget(
  imap,
  file = normalizePath("docs/index.html", mustWork = FALSE),
  selfcontained = FALSE,
  libdir = "index_files"
)

# Comentario con la marca de tiempo al final del HTML
cat(sprintf("\n<!-- Última actualización: %s -->\n", timestamp),
    file = "docs/index.html", append = TRUE)

# Comprobación
stopifnot(file.exists("docs/index.html"), dir.exists("docs/index_files"))