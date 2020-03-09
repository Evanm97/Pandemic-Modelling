setwd("C:/Users/user/OneDrive/FYP")

counties <- geojsonio::geojson_read("Data/Counties.geojson", what = "sp")
england <- geojsonio::geojson_read("Data/England.geojson", what = "sp")
france <- geojsonio::geojson_read("Data/France.geojson", what = "sp")
portugal <- geojsonio::geojson_read("Data/Portugal.geojson", what = "sp")

portugal$name
portugal$population


binsPop <- c(20000, 40000, 80000, 100000, 150000, 180000, 200000,
             250000, 500000, 1000000, Inf)

binsPopFr <- c(200, 20000, 1000000, 2000000, 3000000, 5000000, 6000000, 7000000, 10000000, Inf)

palPop <-
  colorBin("YlOrRd", domain = counties$population, bins = binsPop)

palPopFr <-
  colorBin("YlOrRd", domain = france$name, bins = binsPopFr)


labelsPop <- sprintf("<strong>%s</strong><br/>%g people",
                   counties$name,
                   counties$population) %>% lapply(htmltools::HTML)

binsDen <- c(0, 20, 40, 60, 80, 100, 250, 500, 1000, Inf)
palDen <- colorBin("YlOrRd", domain = counties$id, bins = binsDen)

labelsDen <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  counties$name, counties$density
) %>% lapply(htmltools::HTML)

labelsFr <- sprintf(
  "<strong>%s</strong><br/>%g people",
  france$name, france$population
) %>% lapply(htmltools::HTML)

labelsInf <- sprintf(
  "<strong>%s</strong><br/>%g infected",
  counties$name, as.integer(counties$infected)
) %>% lapply(htmltools::HTML)
