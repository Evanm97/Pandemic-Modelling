library(shiny)
library(ggplot2)
library(scales)
source("Model.R")
library(leaflet)
library(maps)
library(htmlwidgets)
library(geojsonio)


# Setting up the client (page elements appear in order specified)
ui <- fluidPage(
  titlePanel("Pandemic Modeling"),
  br(),br(),
  navbarPage(
    "My Application",
    tabPanel("Population Density", 
            
               radioButtons("choice", "",
                            choices = c(Population = "mapPop", Density = "mapDensity")), mapDensity),
    
    tabPanel("Infected", 
             sidebarPanel(
               sliderInput("infectedtime", "Time", min=1, max=160, value=1, step=1)),
             mainPanel( leafletOutput("infectedmap"))),
    
    tabPanel("Dynamic SIR", sidebarLayout(
      sidebarPanel(
        sliderInput("cr", "Contact Rate", min=0, max=10, value=5, step=1),
        sliderInput("inf", "Infectivity", min=0, max=1, value=0.5, step=0.05),
        sliderInput("rd",  "Recovery Delay", min=1, max=5, value=2, step=1),
        
        #numericInput("obs", "Population Size:", 1000, min = 1, max = 10000000),
        #verbatimTextOutput("value"),
        
        selectInput("state", "Population", selected = "Total",
                    list(Total = list("Total Population" = 4830000),`Munster` = list("Cork"=counties$population[32], "Clare"=counties$population[18], "Kerry"=counties$population[19], "Tipperary"=counties$population[27], "Limerick"=counties$population[29], "Waterford"=counties$population[30]),
                         `Leinster` = list("Carlow"=counties$population[7],"Dublin"=counties$population[28], "Kildare"=counties$population[8], "Kilkenny"=counties$population[9], "Laois"=counties$population[10], "Longford"=counties$population[11],
                                           "Louth"=counties$population[12], "Meath"=counties$population[13], "Offaly"=counties$population[14], "Westmeath"=counties$population[15], "Wexford"=counties$population[16], "Wicklow"=counties$population[17]),
                         `Connacht` = list("Galway"=counties$population[31], "Leitrim"=counties$population[20], "Mayo"=counties$population[21], "Roscommon"=counties$population[22], "Sligo"=counties$population[23]),
                         `Ulster` = list("Derry"=counties$population[4], "Antrim"=counties$population[2], "Down"=counties$population[1], "Tyrone"=counties$population[5],
                                         "Armagh"=counties$population[3], "Fermanagh"=counties$population[6], "Cavan"=counties$population[24], "Monaghan"=counties$population[26], "Donegal"=counties$population[25]))
        ),
        
        
        
      ),
      mainPanel(
        plotOutput("plot"),
        verbatimTextOutput("stats")
      )
    )),
    tabPanel("Data Explorer",
             DT::dataTableOutput("mytable", width = "100%")),
    tabPanel("Dynamic Charts", titlePanel(title = h4("Infected Data", align="center")),
             sidebarPanel(
               
               radioButtons("COUNTY", "Select the County",
                            choices = c(Dublin = "InfectedD", Limerick = "InfectedL", Waterford =
                                          "InfectedW", Cork = "InfectedC", "Galway" = "InfectedG"),
                            selected = "InfectedD"),
               
               sliderInput("slider2", label = h3("Slider Range"), min = 0, 
                           max = 161, value = c(1, 25))),
             
             mainPanel(
               plotOutput("bar",height = 500))),
    tabPanel("SIR",
             plotOutput("plot2"))
  ))

  
# The Server Function
server <- function(input, output) {
  
  data <- reactive({
    cat(file=stderr(), "Function data (reactive function)...\n")
    START<-0; FINISH<-20; STEP<-0.125
    simtime <- seq(START, FINISH, by=STEP)
    stocks  <- c(sSusceptible=as.numeric(input$state)-1,sInfected=1,sRecovered=0)
    auxs    <- c(aTotalPopulation=as.numeric(input$state), 
                 aContactRate=as.numeric(input$cr), 
                 aInfectivity=as.numeric(input$inf),
                 aDelay=as.numeric(input$rd),
                 PPE=as.numeric(0),
                 PPEDELAY=as.numeric(0),
                 quarantineF=as.numeric(0))
    
    o<-data.frame(ode(y=stocks, times=simtime, func = model, 
                      parms=auxs, method="euler"))
  })
  
  output$plot <- renderPlot({
    cat(file=stderr(), "Function output$plot::renderPlot...\n")
    
    o <- data()
    
    ggplot()+
      geom_line(data=o,aes(time,o$sSusceptible,color="1. Susceptible"))+
      geom_line(data=o,aes(time,o$sInfected,color="2. Infected"))+
      geom_line(data=o,aes(time,o$sRecovered,color="3. Recovered"))+
      scale_y_continuous(labels = comma)+
      ylab("System Stocks")+
      xlab("Day") +
      labs(color="")+
      theme(legend.position="bottom")
  })
  
  output$mytable = DT::renderDataTable({
    o2
  })
  
  output$leaflet <- renderLeaflet({input$choice})
  
  output$stats <- renderPrint({
    summary(data()[,c("sSusceptible","sInfected","sRecovered")])
  })
  
  output$bar <- renderPlot({
    
    reData <- subset(o2, select=c(input$COUNTY))
    
    barplot(reData[input$slider2[1]:input$slider2[2],1], main = input$COUNTY,
            names.arg =o2$time[input$slider2[1]:input$slider2[2]], xlab = "Time", ylab = "Infected")
  })
  
  output$plot2 <- renderPlot({
    cat(file=stderr(), "Function output$plot::renderPlot...\n")
    
    o <- data()
    
    ggplot()+
      geom_line(data=o2,size=1,aes(time,o2$InfectedD,color="1. Dublin"))+
      geom_line(data=o2,size=1,aes(time,o2$InfectedG,color="2. Galway"))+
      geom_line(data=o2,size=1,aes(time,o2$InfectedL,color="3. Limerick"))+
      geom_line(data=o2,size=1,aes(time,o2$InfectedC,color="4. Cork"))+
      geom_line(data=o2,size=1,aes(time,o2$InfectedW,color="5. Waterford"))+
      geom_line(data=o2,size=1,aes(time,o2$TotalInfected,color="6. Total"))+
      scale_y_continuous(labels = comma)+
      ylab("Infected")+
      xlab("Month") +
      labs(color="")+
      theme(legend.position="bottom")
  })
  
  output$infectedmap <- renderLeaflet({
    
    c <- data()
    
    counties$infected <- as.integer(rep(0, 32))
    
    counties$infected[28] <- o2$InfectedD[input$infectedtime]
    counties$infected[29] <- o2$InfectedL[input$infectedtime]
    counties$infected[30] <- o2$InfectedW[input$infectedtime]
    counties$infected[31] <- o2$InfectedG[input$infectedtime]
    counties$infected[32] <- o2$InfectedC[input$infectedtime]
    
    # for (value in counties$infected) {
    #   print(value)
    #   counties$infected[28] <- o2$InfectedD[50
    # }
    
    binsInf <- c(100, 1000, 5000, 10000, 25000, 50000, 75000,
                 100000, 200000, 500000, Inf)
    
    palInf <-
      colorBin("Greens", domain = counties$infected, bins = binsInf)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g infected",
      counties$name, counties$infected
    ) %>% lapply(htmltools::HTML)
    
    leaflet(counties) %>%
      setView(-7.77832031, 53.2734, 6) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ palInf(infected),
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
        
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = palInf,values = ~ infected,opacity = 0.7,title = NULL,
                position = "bottomright")
  })
  
}

# Launch the app
shinyApp(ui, server)

 
