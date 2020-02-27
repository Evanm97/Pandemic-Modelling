path = "C:/Users/user/OneDrive/FYP/Data"
path2 = "C:/Users/user/OneDrive/FYP/DataUK"

file <- dir(path, pattern = ".geojson", full.names = TRUE)
file2 <- dir(path2, pattern = ".geojson", full.names = TRUE)

counties <- geojsonio::geojson_read(file, what = "sp")
england <- geojsonio::geojson_read(file2, what = "sp")


binsPop <- c(20000, 40000, 80000, 100000, 150000, 180000, 200000,
             250000, 500000, 1000000, Inf)
palPop <-
  colorBin("YlOrRd", domain = counties$population, bins = binsPop)


labelsPop <- sprintf("<strong>%s</strong><br/>%g people",
                   counties$name,
                   counties$population) %>% lapply(htmltools::HTML)

binsDen <- c(0, 20, 40, 60, 80, 100, 250, 500, 1000, Inf)
palDen <- colorBin("YlOrRd", domain = counties$id, bins = binsDen)

labelsDen <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  counties$name, counties$density
) %>% lapply(htmltools::HTML)
