map <- leaflet(counties) %>%
  setView(-7.77832031, 53.2734, 6) %>%
  addTiles()
map