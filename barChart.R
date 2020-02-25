library(shiny)

ui <- fluidPage(
  titlePanel(title = h4("Infected Data", align="center")),
  sidebarPanel(
    
    radioButtons("COUNTY", "Select the County",
                 choices = c(Dublin = "InfectedD", Limerick = "InfectedL", Waterford =
                               "InfectedW", Cork = "InfectedC", "Galway" = "InfectedG"),
                 selected = "InfectedD"),
  
  sliderInput("slider2", label = h3("Slider Range"), min = 0, 
              max = 161, value = c(1, 25))),
  
  mainPanel(
    plotOutput("bar",height = 500))
)

server <- function(input,output){
 
  output$bar <- renderPlot({
   
    reData <- subset(o2, select=c(input$COUNTY))
    
    barplot(reData[input$slider2[1]:input$slider2[2],1], main = input$COUNTY,
            xlab = "Time", ylab = "Infected")
  })
}
shinyApp(ui=ui, server=server)
