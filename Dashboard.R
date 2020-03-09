## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Pandemic Dashbaord"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("globe-americas")),
      menuItem("Charts", tabName = "charts", icon = icon("chart-bar")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("database")),
      menuItem("SIR", tabName = "sir", icon = icon("chart-line"))
      
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      
      # Dashboard
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("sus"),
                valueBoxOutput("users"),
                valueBoxOutput("rec"),
                #valueBoxOutput("days")
              )
      ),
      
      
      # Data Explorer
      tabItem(tabName = "explorer",
              DT::dataTableOutput("mytable", width = "100%")
      ),
      
      # Bar Chart
      tabItem(tabName = "charts",
              sidebarPanel(
                
                radioButtons("COUNTY", "Select the County",
                             choices = c(Dublin = "InfectedD", Limerick = "InfectedL", Waterford =
                                           "InfectedW", Cork = "InfectedC", "Galway" = "InfectedG"),
                             selected = "InfectedD"),
                
                sliderInput("slider2", label = h3("Slider Range"), min = 0, 
                            max = 161, value = c(1, 25))),
              
              mainPanel(
                plotOutput("bar",height = 500))
      ),
      
      # SIR
      tabItem(tabName = "sir",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("cr", "Contact Rate", min=0, max=10, value=5, step=1),
                  sliderInput("inf", "Infectivity", min=0, max=1, value=0.5, step=0.05),
                  sliderInput("rd",  "Recovery Delay", min=1, max=5, value=2, step=1),
                  
                  sliderInput("qf", "Quarantine Factor:", 0, min = 0, max = 1, step=0.1),
                  
                  selectInput("country", "Country", selected = "Ireland",
                              list("Ireland", "France", "Portugal", "Spain")
                  ),
                  
                  uiOutput("secondSelection"),
                  
                  
                  
                ),
                mainPanel(
                  plotOutput("plot"),
                  verbatimTextOutput("stats")
                )
              )
      ),
      
      # Map
      tabItem(tabName = "map",
              fluidRow( column(12,actionButton("mapP", "Population"),
                               actionButton("mapD", "Pop-Density"),
                               actionButton("map", "No-Filter"),
              ),hr()), leafletOutput("map")
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  v2 <- reactiveValues()
  
  observeEvent(input$country, {
    if (input$country == "Ireland") {
        v2$data2 <- irelandSel
    } else if (input$country == "France") {
        v2$data2 <- franceSel
    } else if (input$country == "Portugal") {
        v2$data2 <- portugalSel
    } else if (input$country == "Spain") {
        v2$data2 <- spainSel
    }
  })
  
  output$secondSelection <- renderUI({
    v2$data2
  })
  
  v <- reactiveValues(data = mapPop)
  
  observeEvent(input$mapP, {
    v$data <- mapPop
  })
  
  observeEvent(input$map, {
    v$data <- map
  })
  
  observeEvent(input$mapD, {
    v$data <- mapDensity
  })
  
  output$map <- renderLeaflet({v$data})
  
  output$mytable = DT::renderDataTable({
    o2
  })
  
  data <- reactive({
    cat(file=stderr(), "Function data (reactive function)...\n")
    START<-0; FINISH<-20; STEP<-0.125
    simtime <- seq(START, FINISH, by=STEP)
    stocks  <- c(sSusceptible=as.numeric(input$state)-1,sInfected=1,sRecovered=0)
    auxs    <- c(aTotalPopulation=as.numeric(input$state), 
                 aContactRate=as.numeric(input$cr), 
                 aInfectivity=as.numeric(input$inf),
                 aDelay=as.numeric(input$rd),
                 quarantineF=as.numeric(input$qf))
                 
    
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
  
  output$stats <- renderPrint({
    summary(data()[,c("sSusceptible","sInfected","sRecovered")])
  })
  
  output$bar <- renderPlot({
    
    reData <- subset(o2, select=c(input$COUNTY))
    
    barplot(reData[input$slider2[1]:input$slider2[2],1], main = input$COUNTY,
            names.arg =o2$time[input$slider2[1]:input$slider2[2]], xlab = "Time", ylab = "Infected")
  })
  
  output$sus <- renderValueBox({
    valueBox(
      value = 20000,
      subtitle = "Suceptible",
      icon = icon("user-alt")
    )
  })
  
  output$users <- renderValueBox({
    valueBox(
      11750,
      "Infected",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$rec <- renderValueBox({
    valueBox(
      11750,
      "Recovered",
      icon = icon("procedures"),
      color = "orange"
    )
  })
  
  output$days <- renderValueBox({
    valueBox(
      4,
      "Days",
      icon = icon("calendar-alt")
    )
  })
  
}

shinyApp(ui, server)
