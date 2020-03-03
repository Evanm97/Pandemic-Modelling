library(leaflet)
library(maps)
library(htmlwidgets)
library(geojsonio)

mapFr <- leaflet(france) %>%
  setView(-7.77832031, 53.2734, 6) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~ palPopFr(population),
    #fillColor = "blue",
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
      bringToFront = TRUE
    ),
    label = labelsFr,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ))
  # ) %>%
  # addLegend(pal = palPopFr,values = ~ population,opacity = 0.7,title = NULL,
  #           position = "bottomright")

mapFr
