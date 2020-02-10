library(leaflet)
library(maps)
library(htmlwidgets)
library(geojsonio)

path = "C:/Users/user/OneDrive/FYP/Data"
file <- dir(path, pattern = ".geojson", full.names = TRUE)
counties <- geojsonio::geojson_read(file, what = "sp")


binsDen <- c(0, 20, 40, 60, 80, 100, 250, 500, 1000, Inf)
palDen <- colorBin("YlOrRd", domain = counties$id, bins = binsDen)

labels3 <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  counties$name, counties$density
) %>% lapply(htmltools::HTML)

mapDensity <- leaflet(counties) %>%
  setView(-7.77832031,53.2734,6) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palDen(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels3,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = palDen, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright")

