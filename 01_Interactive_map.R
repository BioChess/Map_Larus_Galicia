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

# Filtrar datos de los últimos 2 días
lst_days <- as.POSIXct(Sys.time(), tz="GMT") - (2 * 24 * 60 * 60)  # Hace 2 días
gps.df2 <- gps.df %>% filter(datetimeGMT >= lst_days)

# Crear la paleta de colores para cada birdID
pal.colors <- colorFactor(palette = "Set1", domain = gps.df$birdID)

# Crear el mapa base
leafletOptions <- leaflet::leafletOptions(preferCanvas = TRUE)
imap <- leaflet(options = leafletOptions) %>%
  addTiles(options = tileOptions(maxZoom = 10))

# Agregar capas de tracks por cada individuo
grupos <- unique(gps.df$birdID)  # Obtener los ID únicos

for (bird in grupos) {
  # print(bird)
  gps.ind <- gps.df2 %>% filter(birdID == bird)  # Filtrar datos por birdID
  
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
      group = bird  # Asigna el track al mismo grupo
    ) %>%
    addCircleMarkers(
      lng = ~longitude, lat = ~latitude, 
      data = gps.ind,
      radius = 0.5, color = ~pal.colors(birdID), 
      popup = ~paste("ID:", birdID, "<br>Fecha:", datetimeGMT, "Sex:", sex),
      group = bird  # Asigna un grupo con el nombre del birdID
    ) %>% 
    # Agregar la última posición como una estrella
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
# Agregar control de capas para activar/desactivar individuos
#imap <- imap %>%
#  addLayersControl(
#    overlayGroups = grupos,  # Usa los birdID como grupos de control
#    options = layersControlOptions(collapsed = FALSE)  # Mostrar la lista expandida
#  )

# Añadir una marca de tiempo como comentario al final del HTML
timestamp <- format(Sys.time(), "%Y%m%d_%H%M", tz = "GMT", usetz = TRUE)

# Crear texto HTML para mostrar en el mapa
update_label <- paste0("Última actualización: ", format(Sys.time(), "%d-%m-%Y %H:%M", tz = "GMT", usetz = TRUE))

# Añadir el control al mapa (abajo a la derecha)
imap <- imap %>%
  addControl(html = update_label, position = "topright")

if (length(imap$x$calls) == 0) {
  imap <- imap %>% addControl(html = "Sin datos recientes para mostrar", position = "topright")
}

if (file.exists("docs/index.html")) file.remove("docs/index.html")
if (dir.exists("docs/index_files")) unlink("docs/index_files", recursive = TRUE)

# Guardar el mapa correctamente
if (!dir.exists("docs")) {
  dir.create("docs")
}
saveWidget(imap, file = "docs/index.html", selfcontained = TRUE)
#saveWidget(imap, file = "docs/index.html", selfcontained = FALSE, libdir = "docs/index_files")
cat(sprintf("\n<!-- Última actualización: %s -->\n", timestamp),
    file = "docs/index.html", append = TRUE)






