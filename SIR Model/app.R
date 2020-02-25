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
    tabPanel("Population Density", mapDensity),
    
    tabPanel("Population", mapPop),
    tabPanel("SIR", sidebarLayout(
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
               plotOutput("bar",height = 500)))
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
                 aDelay=as.numeric(input$rd))
    
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
  
  output$stats <- renderPrint({
    summary(data()[,c("sSusceptible","sInfected","sRecovered")])
  })
  
  output$bar <- renderPlot({
    
    reData <- subset(o2, select=c(input$COUNTY))
    
    barplot(reData[input$slider2[1]:input$slider2[2],1], main = input$COUNTY,
            names.arg =o2$time[input$slider2[1]:input$slider2[2]], xlab = "Time", ylab = "Infected")
  })
}

# Launch the app
shinyApp(ui, server)

 
