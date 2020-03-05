
irelandSel <- selectInput("state", "Population", selected = "Total",
            list(Total = list("Total Population" = 4830000),`Munster` = list("Cork"=counties$population[32], "Clare"=counties$population[18], "Kerry"=counties$population[19], "Tipperary"=counties$population[27], "Limerick"=counties$population[29], "Waterford"=counties$population[30]),
                 `Leinster` = list("Carlow"=counties$population[7],"Dublin"=counties$population[28], "Kildare"=counties$population[8], "Kilkenny"=counties$population[9], "Laois"=counties$population[10], "Longford"=counties$population[11],
                                   "Louth"=counties$population[12], "Meath"=counties$population[13], "Offaly"=counties$population[14], "Westmeath"=counties$population[15], "Wexford"=counties$population[16], "Wicklow"=counties$population[17]),
                 `Connacht` = list("Galway"=counties$population[31], "Leitrim"=counties$population[20], "Mayo"=counties$population[21], "Roscommon"=counties$population[22], "Sligo"=counties$population[23]),
                 `Ulster` = list("Derry"=counties$population[4], "Antrim"=counties$population[2], "Down"=counties$population[1], "Tyrone"=counties$population[5],
                                 "Armagh"=counties$population[3], "Fermanagh"=counties$population[6], "Cavan"=counties$population[24], "Monaghan"=counties$population[26], "Donegal"=counties$population[25]))
)


franceSel <- selectInput("state", "Population", selected = "Total",
            list(Total = list("Total Population" = 50000000),`Region` = list("Île-de-France"=france$population[1], "Centre-Val de Loire"=france$population[2], "Bourgogne-Franche-Comté"=france$population[3], 
                 "Normandie"=france$population[4], "Hauts-de-France"=france$population[5], "Grand Est"=france$population[6], "Pays de la Loire"=france$population[7], "Bretagne"=france$population[8],
                 "Nouvelle-Aquitaine"=france$population[9], "Guadeloupe"=france$population[10], "Martinique"=france$population[11], "Guyane"=france$population[12], "La Réunion"=france$population[13],
                 "Mayotte"=france$population[14], "Occitanie"=france$population[15], "Auvergne-Rhône-Alpes"=france$population[16], "Provence-Alpes-Côte d'Azur"=france$population[17], "Corse"=france$population[18])))
