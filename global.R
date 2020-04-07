library(shiny)
library(ggplot2)
library(scales)
library(shinyMatrix)
library(leaflet)
library(maps)
library(htmlwidgets)
library(geojsonio)
library(spData)

ireland <- geojsonio::geojson_read("Data/Ireland.geojson", what = "sp")
england <- geojsonio::geojson_read("Data/England.geojson", what = "sp")
france <- geojsonio::geojson_read("Data/France.geojson", what = "sp")
portugal <- geojsonio::geojson_read("Data/Portugal.geojson", what = "sp")
spain <- geojsonio::geojson_read("Data/Spain.geojson", what = "sp")

ireland$infected <- as.integer(rep(0, 32))
france$infected <- as.integer(rep(0, 18))
portugal$infected <- as.integer(rep(0, 18))
spain$infected <- as.integer(rep(0, 19))

countries <- c(ireland, france, portugal, spain)

binsInfIre <- c(100, 1000, 5000, 10000, 25000, 50000, 75000,
                100000, 200000, 500000, Inf)

binsInfFr <- c(100, 1000, 10000, 50000, 100000, 250000, 500000, 750000,
               1000000, 2000000, 5000000, 6000000, 8000000, 10000000, 15000000, 30000000, Inf)

binsInfPor <- c(100, 1000, 10000, 50000, 100000, 250000, 500000, 750000,
               1000000, 2000000, 5000000, 6000000, 8000000, 10000000, Inf)

binsInfSp <- c(100, 1000, 10000, 50000, 100000, 250000, 500000, 750000,
                1000000, 2000000, 5000000, 6000000, 8000000, 10000000, Inf)

palInfIre <-
  colorBin("Greens", domain = ireland$infected, bins = binsInfIre)

palInfFr <-
  colorBin("Greens", domain = france$infected, bins = binsInfFr)

palInfPor <-
  colorBin("Greens", domain = portugal$infected, bins = binsInfPor)

palInfSp <-
  colorBin("Greens", domain = spain$infected, bins = binsInfSp)

labelopts <- labelOptions(
  style = list("font-weight" = "normal", padding = "3px 8px"),
  textsize = "15px",
  direction = "auto"
)

highlightopts <- highlightOptions(
  weight = 5,
  color = "#666",
  dashArray = "",
  fillOpacity = 0.7,
  bringToFront = TRUE
)

irelandSel <- selectInput("state", "Population", selected = "Total",
                          list(Total = list("Total Population" = 4830000),`Munster` = list("Cork"=ireland$population[32], "Clare"=ireland$population[18], "Kerry"=ireland$population[19], "Tipperary"=ireland$population[27], "Limerick"=ireland$population[29], "Waterford"=ireland$population[30]),
                               `Leinster` = list("Carlow"=ireland$population[7],"Dublin"=ireland$population[28], "Kildare"=ireland$population[8], "Kilkenny"=ireland$population[9], "Laois"=ireland$population[10], "Longford"=ireland$population[11],
                                                 "Louth"=ireland$population[12], "Meath"=ireland$population[13], "Offaly"=ireland$population[14], "Westmeath"=ireland$population[15], "Wexford"=ireland$population[16], "Wicklow"=ireland$population[17]),
                               `Connacht` = list("Galway"=ireland$population[31], "Leitrim"=ireland$population[20], "Mayo"=ireland$population[21], "Roscommon"=ireland$population[22], "Sligo"=ireland$population[23]),
                               `Ulster` = list("Derry"=ireland$population[4], "Antrim"=ireland$population[2], "Down"=ireland$population[1], "Tyrone"=ireland$population[5],
                                               "Armagh"=ireland$population[3], "Fermanagh"=ireland$population[6], "Cavan"=ireland$population[24], "Monaghan"=ireland$population[26], "Donegal"=ireland$population[25]))
)

franceSel <- selectInput("state", "Population", selected = "Total",
                         list(Total = list("Total Population" = 50000000),`Region` = list("Île-de-France"=france$population[1], "Centre-Val de Loire"=france$population[2], "Bourgogne-Franche-Comté"=france$population[3],
                                                                                          "Normandie"=france$population[4], "Hauts-de-France"=france$population[5], "Grand Est"=france$population[6], "Pays de la Loire"=france$population[7], "Bretagne"=france$population[8],
                                                                                          "Nouvelle-Aquitaine"=france$population[9], "Guadeloupe"=france$population[10], "Martinique"=france$population[11], "Guyane"=france$population[12], "La Réunion"=france$population[13],
                                                                                          "Mayotte"=france$population[14], "Occitanie"=france$population[15], "Auvergne-Rhône-Alpes"=france$population[16], "Provence-Alpes-Côte d'Azur"=france$population[17], "Corse"=france$population[18])))

portugalSel <- selectInput("state", "Population", selected = "Total",
                           list(Total = list("Total Population" = 10250000),`Region` = list("Aveiro"=portugal$population[1], "Beja"=portugal$population[2], "Braga"=portugal$population[3],
                                                                                            "Castelo Branco"=portugal$population[4], "Coimbra"=portugal$population[5], "Guarda"=portugal$population[6], "Leiria"=portugal$population[7], "Lisbon"=portugal$population[8],
                                                                                            "Portalegre"=portugal$population[9], "Porto"=portugal$population[10], "Viana do Castelo"=portugal$population[11], "Vila Real"=portugal$population[12], "Viseu"=portugal$population[13],
                                                                                            "Bragança"=portugal$population[14], "Évora"=portugal$population[15], "Santarém"=portugal$population[16], "Setúbal"=portugal$population[17], "Faro"=portugal$population[18])))

spainSel <- selectInput("state", "Population", selected = "Total",
                        list(Total = list("Total Population" = 46660000),`Region` = list("Castilla-Leon"=spain$population[1], "Cataluña"=spain$population[2], "Ceuta"=spain$population[3],
                                                                                         "Murcia"=spain$population[4], "La Rioja"=spain$population[5], "Baleares"=spain$population[6], "Canarias"=spain$population[7], "Cantabria"=spain$population[8],
                                                                                         "Andalucia"=spain$population[9], "Asturias"=spain$population[10], "Valencia"=spain$population[11], "Melilla"=spain$population[12], "Navarra"=spain$population[13],
                                                                                         "Galicia"=spain$population[14], "Aragon"=spain$population[15], "Madrid"=spain$population[16], "Extremadura"=spain$population[17], "Castilla-La Mancha"=spain$population[18], "Pais Vasco"=spain$population[19])))

irelandSel2 <- selectInput("state_ireland", "Origin of Infection",
                           list(`Munster` = list("Cork" = 32, "Clare"= 18, "Kerry"= 19, "Tipperary"= 27, "Limerick"= 29, "Waterford"= 30),
                                `Leinster` = list("Carlow"= 7,"Dublin"= 28, "Kildare"= 8, "Kilkenny"= 9, "Laois"= 10, "Longford"= 11,
                                                  "Louth"= 12, "Meath"= 13, "Offaly"= 14, "Westmeath"= 15, "Wexford"= 16, "Wicklow"= 17),
                                `Connacht` = list("Galway"= 31, "Leitrim"= 20, "Mayo"= 21, "Roscommon"= 22, "Sligo"= 23),
                                `Ulster` = list("Derry" = 4, "Antrim" = 2, "Down" = 1 , "Tyrone" = 5,
                                                "Armagh" = 3, "Fermanagh" = 6, "Cavan" = 24, "Monaghan" = 26, "Donegal" = 25 ))
)

franceSel2 <- selectInput("state_france", "Origin of Infection",
                          list(`Region` = list("Île-de-France"=1, "Centre-Val de Loire"=2, "Bourgogne-Franche-Comté"=3,
                                               "Normandie"=4, "Hauts-de-France"=5, "Grand Est"=6, "Pays de la Loire"=7, "Bretagne"=8,
                                               "Nouvelle-Aquitaine"=9, "Guadeloupe"=10, "Martinique"=11, "Guyane"=12, "La Réunion"=13,
                                               "Mayotte"=14, "Occitanie"=15, "Auvergne-Rhône-Alpes"=16, "Provence-Alpes-Côte d'Azur"=17, "Corse"=18)))

portugalSel2 <- selectInput("state_portugal", "Origin of Infection",
                            list(`Region`=list("Aveiro"=1, "Beja"=2, "Braga"=3,
                                               "Castelo Branco"=4, "Coimbra"=5, "Guarda"=6, "Leiria"=7, "Lisbon"=8,
                                               "Portalegre"=9, "Porto"=10, "Viana do Castelo"=11, "Vila Real"=12, "Viseu"=13,
                                               "Bragança"=14, "Évora"=15, "Santarém"=16, "Setúbal"=17, "Faro"=18)))

spainSel2 <- selectInput("state_spain", "Origin of Infection",
                         list(`Region` = list("Castilla-Leon"=1, "Cataluña"=2, "Ceuta"=3,
                                              "Murcia"=4, "La Rioja"=5, "Baleares"=6, "Canarias"=7, "Cantabria"=8,
                                              "Andalucia"=9, "Asturias"=10, "Valencia"=11, "Melilla"=12, "Navarra"=13,
                                              "Galicia"=14, "Aragon"=15, "Madrid"=16, "Extremadura"=17, "Castilla-La Mancha"=18, "Pais Vasco"=19)))


