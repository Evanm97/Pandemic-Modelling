library(leaflet)
library(maps)
library(htmlwidgets)

path = "C:/Users/user/OneDrive/FYP/Data"
file <- dir(path, pattern = ".geojson", full.names = TRUE)
counties <- geojsonio::geojson_read(file, what = "sp")

binsPop <- c(20000, 40000, 80000, 100000, 150000, 180000, 200000,
             250000, 500000, 1000000, Inf)
palPop <-
  colorBin("YlOrRd", domain = counties$population, bins = binsPop)


labels2 <- sprintf("<strong>%s</strong><br/>%g people",
                   counties$name,
                   counties$population) %>% lapply(htmltools::HTML)

mapPop <- leaflet(counties) %>%
  setView(-7.77832031, 53.2734, 6) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~ palPop(population),
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
    label = labels2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = palPop,
    values = ~ population,
    opacity = 0.7,
    title = NULL,
    position = "bottomright"
  )
