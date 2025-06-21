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
lst_days <- as.POSIXct(Sys.time(), tz="GMT") - (3 * 24 * 60 * 60)  # Hace 2 días
gps.df2 <- gps.df %>% filter(datetimeGMT >= lst_days)

# Crear la paleta de colores para cada birdID
pal.colors <- colorFactor(palette = "Set1", domain = gps.df$birdID)

# Crear las paletas de colores para LARFUS y LARMIC
orange_palette <- colorRampPalette(c("gold4", "darkorange"))(10)
green_palette <- colorRampPalette(c("lightgreen", "darkgreen"))(10)

# Elige un color representativo de cada paleta
color_larfus<- 'darkgreen'
color_larmic <- 'orange'

# Función para asignar colores según el prefijo del birdID
get_color <- function(birdID) {
  if (startsWith(as.character(birdID), "6")) {
    # LARFUS: BirdID empieza con "6"
    return(sample(green_palette, 1))
  } else if (startsWith(as.character(birdID), "7")) {
    # LARMIC: BirdID empieza con "7"
    return(sample(orange_palette, 1))
  }
  return("grey")  # Color por defecto para otros casos
}

# Crear el mapa base
leafletOptions <- leaflet::leafletOptions(preferCanvas = TRUE)
imap <- leaflet(options = leafletOptions) %>%
  addProviderTiles("Esri.WorldImagery", group = "Satélite")

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
  color <- get_color(bird)  # Obtener el color basado en el prefijo del birdID
  
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
      radius = 0.5, color = color, 
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
update_label <- paste0("Last update: ", format(Sys.time(), "%d-%m-%Y %H:%M", tz = "GMT", usetz = TRUE))

# Añadir el control al mapa (abajo a la derecha)
imap <- imap %>%
  addControl(html = update_label, position = "topright")%>%
  addLegend(
    position = "bottomright",
    colors = c(color_larfus, color_larmic),
    labels = c("Lesser black-backed gull", "Yellow-legged gull"),
    title = "Species",
    opacity = 1#,
    #className = "custom-legend"
  )

if (length(imap$x$calls) == 0) {
  imap <- imap %>% addControl(html = "Sin datos recientes para mostrar", position = "topright")
}

if (file.exists("docs/index.html")) file.remove("docs/index.html")
if (dir.exists("docs/index_files")) unlink("docs/index_files", recursive = TRUE)

# Guardar el mapa correctamente
# Guardar el mapa correctamente
if (!dir.exists("docs")) {
  dir.create("docs", recursive = TRUE, showWarnings = FALSE)
}

# Modifica la parte final del script donde guardas el mapa:
tryCatch({
  saveWidget(imap, 
             file = "docs/index.html", 
             selfcontained = FALSE, 
             libdir = "index_files",  # Importante: sin la ruta docs/
             title = "Mapa Larus Galicia")
  
  # Mover los archivos a la ubicación correcta
  if(dir.exists("index_files")) {
    file.rename("index_files", "docs/index_files")
  }
  
  cat(sprintf("\n<!-- Última actualización: %s -->\n", timestamp), 
      file = "docs/index.html", append = TRUE)
}, error = function(e) {
  message("ERROR al guardar el widget: ", e$message)
  quit(status = 1)
})




