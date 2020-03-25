library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
source("models.R")
library(DT)

ui <- dashboardPage(

  dashboardHeader(title = "Pandemic Dashbaord"),

  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Infected Map", tabName = "infectedmap", icon = icon("globe-americas")),
      menuItem("Data Explorer", icon = icon("database"),
               menuSubItem("Ireland", tabName = "irelandtable", icon = icon("database")),
               menuSubItem("France", tabName = "francetable", icon = icon("database")),
               menuSubItem("Spain", tabName = "spaintable", icon = icon("database")),
               menuSubItem("Portugal", tabName = "portugaltable", icon = icon("database"))),

      menuItem("SIR", icon = icon("chart-line"),
               menuSubItem(
                 "Vectorized SIR", tabName = "sir2", icon = icon("project-diagram")
               ),
               menuSubItem("Dynamic SIR", tabName = "dsir", icon = icon("sliders-h"))
      )
    )
  ),

  # Body content
  dashboardBody(
    tabItems(

      # Dashboard
      tabItem(tabName = "dashboard",
              tabsetPanel(tabPanel("Ireland", hr(),fluidRow(
                valueBoxOutput("sus_ireland"),
                valueBoxOutput("inf_ireland"),
                valueBoxOutput("rec_ireland"),
                column(4,),
                valueBoxOutput("time_ireland")
              )),

      tabPanel("France", hr(),
               fluidRow(
                 valueBoxOutput("sus_france"),
                 valueBoxOutput("inf_france"),
                 valueBoxOutput("rec_france"),
                 column(4,),
                 valueBoxOutput("time_france")
               )),
      tabPanel("Spain", hr(),
               fluidRow(
                  valueBoxOutput("sus_spain"),
                  valueBoxOutput("inf_spain"),
                  valueBoxOutput("rec_spain"),
                  column(4,),
                  valueBoxOutput("time_spain")
                        )),
     tabPanel("Portugal", hr(),
              fluidRow(
                  valueBoxOutput("sus_portugal"),
                  valueBoxOutput("inf_portugal"),
                  valueBoxOutput("rec_portugal"),
                  column(4,),
                  valueBoxOutput("time_portugal"))))),

      # Data Explorer
      tabItem(tabName = "irelandtable",
              DT::dataTableOutput("tableIreland", width = "100%")
      ),
      tabItem(tabName = "francetable",
              DT::dataTableOutput("tableFrance", width = "100%")
      ),
      tabItem(tabName = "spaintable",
              DT::dataTableOutput("tableSpain", width = "100%")
      ),
      tabItem(tabName = "portugaltable",
              DT::dataTableOutput("tablePortugal", width = "100%")
      ),

      # D-SIR
      tabItem(tabName = "dsir",
              tags$head(
                tags$style(HTML("hr {border-top: 1px solid #add8e6;}"))
              ),
              sidebarLayout(
                sidebarPanel(

                  bsCollapse(id = "collapseExample", open = "Panel 2",
                             bsCollapsePanel("Simtime",
                                             numericInput("d_s", "Start", min = 0, value = 1),
                                             numericInput("d_f", "Finish", min = 0, value = 20),
                                             numericInput("d_st", "Step", min = 0, value = 0.125), style = "success"),

                             bsCollapsePanel("Parameters",
                                             sliderInput("cr", "Contact Rate", min = 0, max = 10, value = 5, step = 1),
                                             sliderInput("inf", "Infectivity", min = 0, max = 1, value = 0.5, step = 0.05),
                                             sliderInput("rd", "Recovery Delay", min = 1, max = 5, value = 2, step = 1), style = "info"),

                             bsCollapsePanel("Countermeasures", sliderInput("qf", "Quarantine Factor:",
                                                                            0, min = 0, max = 1, step = 0.1),
                                             switchInput("ppea", "PPE Flag", value = 0, offStatus = "danger", labelWidth = "100px"),
                                             numericInput("ppe", "PPE's Available:", value = 0, min = 0),
                                             sliderInput("pped", "PPE Allocation Delay", min = 2, max = 10, value = 2, step = 2), style = "danger")),

                  selectInput("country", "Country", selected = "Ireland",
                              list("Ireland", "France", "Portugal", "Spain")
                  ),

                  uiOutput("secondSelection"),


                ),
                mainPanel(
                  plotOutput("plot"), hr(),
                  verbatimTextOutput("stats")
                )
              )
      ),

      # Matrix SIR
      tabItem(tabName = "sir2", h2("Matrix SIR"),
              tabsetPanel(
                tabPanel("SIMTIME", hr(),

                         fluidRow(
                           column(3,
                                  h5("Ireland"),
                                  numericInput("start_ireland", "Start", min = 1, value = 1),
                                  numericInput("finish_ireland", "Finish", min = 1, value = 20),
                                  numericInput("step_ireland", "Step", min = 0, value = 0.125)
                           ),
                           column(3,
                                  h5("France"),
                                  numericInput("start_france", "Start", min = 1, value = 1),
                                  numericInput("finish_france", "Finish", min = 1, value = 20),
                                  numericInput("step_france", "Step", min = 0, value = 0.125)
                           ),
                           column(3,
                                  h5("Portugal"),
                                  numericInput("start_portugal", "Start", min = 1, value = 1),
                                  numericInput("finish_portugal", "Finish", min = 1, value = 20),
                                  numericInput("step_portugal", "Step", min = 0, value = 0.125)
                           ),
                           column(3,
                                  h5("Spain"),
                                  numericInput("start_spain", "Start", min = 1, value = 1),
                                  numericInput("finish_spain", "Finish", min = 1, value = 20),
                                  numericInput("step_spain", "Step", min = 0, value = 0.125)
                           ))),
                tabPanel("Infection Params", hr(),

                         fluidRow(
                           column(3,
                                  h5("Ireland"),
                                  irelandSel2,
                                  numericInput("infno_ireland", "Initial Infected", min = 1, value = 1),
                                  sliderInput("rdm_ireland", "Recovery Delay", min = 1, max = 14, value = 2, step = 1)

                           ),
                           column(3,
                                  h5("France"),
                                  franceSel2,
                                  numericInput("infno_france", "Initial Infected", min = 1, value = 1),
                                  sliderInput("rdm_france", "Recovery Delay", min = 1, max = 14, value = 2, step = 1)
                           ),
                           column(3,
                                  h5("Portugal"),
                                  portugalSel2,
                                  numericInput("infno_portugal", "Initial Infected", min = 1, value = 1),
                                  sliderInput("rdm_portugal", "Recovery Delay", min = 1, max = 14, value = 2, step = 1)
                           ),
                           column(3,
                                  h5("Spain"),
                                  spainSel2,
                                  numericInput("infno_spain", "Initial Infected", min = 1, value = 1),
                                  sliderInput("rdm_spain", "Recovery Delay", min = 1, max = 14, value = 2, step = 1)))),
                         hr())),

      tabItem(tabName = "infectedmap",

              checkboxGroupButtons(
                inputId = "countryc",
                choices = c("Ireland", "France", "Portugal", "Spain"),
                justified = TRUE, status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
              ),
              sliderInput("infectedtime", "Time", min=1, max=160, value=1, step=1, width = "100%", animate = TRUE),
              leafletOutput("infectedmap"))

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

  output$map <- renderLeaflet({v$data})

  output$tableIreland = DT::renderDataTable({
    o <- dataIreland()
   colnames(o) <- c("Time", paste(ireland$name, "(S)"), paste(ireland$name, "(I)"), paste(ireland$name, "(R)"))
   datatable(o, options = list(searching = FALSE,pageLength = 25,lengthMenu = c(25, 50, 100, 150), scrollX = T))
  })

  output$tableFrance = DT::renderDataTable({
    o <- dataFrance()
   colnames(o) <- c("Time", paste(france$name, "(S)"), paste(france$name, "(I)"), paste(france$name, "(R)"))
   datatable(o, options = list(searching = FALSE,pageLength = 25,lengthMenu = c(25, 50, 100, 150), scrollX = T))
  })

  output$tableSpain = DT::renderDataTable({
    o <- dataSpain()
   colnames(o) <- c("Time", paste(spain$name, "(S)"), paste(spain$name, "(I)"), paste(spain$name, "(R)"))
   datatable(o, options = list(searching = FALSE,pageLength = 25,lengthMenu = c(25, 50, 100, 150), scrollX = T))
  })

  output$tablePortugal = DT::renderDataTable({
    o <- dataPortugal()
   colnames(o) <- c("Time", paste(portugal$name, "(S)"), paste(portugal$name, "(I)"), paste(portugal$name, "(R)"))
   datatable(o, options = list(searching = FALSE,pageLength = 25,lengthMenu = c(25, 50, 100, 150), scrollX = T))
  })

  data <- reactive({
    cat(file=stderr(), "Function data (reactive function)...\n")
    START <- as.numeric(input$d_s); FINISH <- as.numeric(input$d_f); STEP <- as.numeric(input$d_st)
    simtime <- seq(START, FINISH, by=STEP)
    stocks  <- c(sSusceptible=as.numeric(input$state)-1,sInfected=1,sRecovered=0, PPE=as.numeric(input$ppe), sInfected_PPE=0)
    auxs    <- c(aTotalPopulation=as.numeric(input$state),
                 aContactRate=as.numeric(input$cr),
                 aInfectivity=as.numeric(input$inf),
                 aDelay=as.numeric(input$rd),
                 aInfectivity_PPE=as.numeric(input$inf*.6),
                 PPEDELAY=as.numeric(input$pped),
                 PPEFLAG=as.numeric(input$ppea),
                 quarantineF=as.numeric(input$qf))

    data.frame(ode(y=stocks, times=simtime, func = model_d, parms=auxs, method="euler"))
  })

  dataIreland <- reactive({

    START <- as.numeric(input$start_ireland)
    FINISH <- as.numeric(input$finish_ireland)
    STEP <- as.numeric(input$step_ireland)

    simtime <- seq(START, FINISH, by=STEP)
    stocks <- c(countries[[as.numeric(1)]][[2]],c(rep(0, (as.numeric(input$state_ireland))-1), rep(input$infno_ireland, 1), rep(0, (32)-(as.numeric(input$state_ireland)))), rep(0, 32))
    auxs <- c(CE=as.numeric(7),
              countryNo=as.numeric(1),
              NUM_COHORTS=as.numeric(32),
              RECOVERY_DELAY=as.numeric(input$rdm_ireland))

    data.frame(ode(y=stocks, times=simtime, func = modelIreland,
                   parms=auxs, method="euler"))
  })

  dataFrance <- reactive({

    START <- as.numeric(input$start_france)
    FINISH <- as.numeric(input$finish_france)
    STEP <- as.numeric(input$step_france)

    simtime <- seq(START, FINISH, by=STEP)
    stocks <- c(countries[[as.numeric(2)]][[2]],c(rep(0, (as.numeric(input$state_france))-1), rep(input$infno_france, 1), rep(0, (18)-(as.numeric(input$state_france)))), rep(0, 18))
    auxs <- c(CE=as.numeric(2),
              countryNo=as.numeric(2),
              NUM_COHORTS=as.numeric(18),
              RECOVERY_DELAY=as.numeric(input$rdm_france))

    data.frame(ode(y=stocks, times=simtime, func = modelFandP, parms=auxs, method="euler"))
  })

  dataSpain <- reactive({

    START <- as.numeric(input$start_spain)
    FINISH <- as.numeric(input$finish_spain)
    STEP <- as.numeric(input$step_spain)

    simtime <- seq(START, FINISH, by=STEP)
    stocks <- c(countries[[as.numeric(4)]][[2]],c(rep(0, (as.numeric(input$state_spain))-1), rep(input$infno_spain, 1), rep(0, (19)-(as.numeric(input$state_spain)))), rep(0, 19))
    auxs <- c(CE=as.numeric(2),
              countryNo=as.numeric(4),
              NUM_COHORTS=as.numeric(19),
              RECOVERY_DELAY=as.numeric(input$rdm_spain))

    data.frame(ode(y=stocks, times=simtime, func = modelSpain, parms=auxs, method="euler"))
  })


  dataPortugal <- reactive({

    START <- as.numeric(input$start_portugal)
    FINISH <- as.numeric(input$finish_portugal)
    STEP <- as.numeric(input$step_portugal)

    simtime <- seq(START, FINISH, by=STEP)
    stocks <- c(countries[[as.numeric(3)]][[2]],c(rep(0, (as.numeric(input$state_portugal))-1), rep(input$infno_portugal, 1), rep(0, (18)-(as.numeric(input$state_portugal)))), rep(0, 18))
    auxs <- c(CE=as.numeric(2),
              countryNo=as.numeric(3),
              NUM_COHORTS=as.numeric(18),
              RECOVERY_DELAY=as.numeric(input$rdm_portugal))

    data.frame(ode(y=stocks, times=simtime, func = modelFandP,
                   parms=auxs, method="euler"))
  })

  # output$statsV <- renderPrint({
  #   summary(dataIreland()[,c("dS_dt", "dI_dt", "dR_dt")])
  # })


  output$plot <- renderPlot({
    cat(file=stderr(), "Function output$plot::renderPlot...\n")

    o <- data()

    ggplot()+
      geom_line(data=o,aes(time,o$sSusceptible,color="1. Susceptible"))+
      geom_line(data=o,aes(time,o$sInfected,color="2. Infected"))+
      geom_line(data=o,aes(time,o$sRecovered,color="3. Recovered"))+
      geom_line(data=o,aes(time,o$PPE,color="4. PPE"))+

      scale_y_continuous(labels = comma)+
      ylab("System Stocks")+
      xlab("Day") +
      labs(color="")+
      theme(legend.position="bottom")
  })

  output$stats <- renderPrint({
    summary(data()[,c("sSusceptible","sInfected","sRecovered", "PPE" )])
  })

  output$sus_ireland <- renderValueBox({
    o <- dataIreland()
    o$TOTAL <-o$X2 + o$X3 + o$X4 + o$X5 + o$X6 + o$X7 + o$X8 + o$X9
    + o$X10 + o$X11 + o$X12 + o$X13 + o$X14 + o$X15 + o$X16 + o$X17
    + o$X18 + o$X19 + o$X20 + o$X21 + o$X22 + o$X23 + o$X24 + o$X25 + o$X26
    + o$X27 + o$X28 + o$X29 + o$X30 + o$X31 + o$X32 + o$X33

    valueBox(
      value = as.integer(o$TOTAL[input$infectedtime]),
      subtitle = "Suceptible",
      icon = icon("user-alt")
    )
  })

  output$sus_france <- renderValueBox({
    o <- dataFrance()
    o$TOTAL <-o$X2 + o$X3 + o$X4 + o$X5 + o$X6 + o$X7 + o$X8 + o$X9
    + o$X10 + o$X11 + o$X12 + o$X13 + o$X14 + o$X15 + o$X16 + o$X17
    + o$X18 + o$X19

    valueBox(
      value = as.integer(o$TOTAL[input$infectedtime]),
      subtitle = "Suceptible",
      icon = icon("user-alt")
    )
  })

  output$sus_spain <- renderValueBox({
    o <- dataSpain()
    o$TOTAL <-o$X2 + o$X3 + o$X4 + o$X5 + o$X6 + o$X7 + o$X8 + o$X9
    + o$X10 + o$X11 + o$X12 + o$X13 + o$X14 + o$X15 + o$X16 + o$X17
    + o$X18 + o$X19 + o$X20

    valueBox(
      value = as.integer(o$TOTAL[input$infectedtime]),
      subtitle = "Suceptible",
      icon = icon("user-alt")
    )
  })

  output$sus_portugal <- renderValueBox({
    o <- dataPortugal()
    o$TOTAL <-o$X2 + o$X3 + o$X4 + o$X5 + o$X6 + o$X7 + o$X8 + o$X9
    + o$X10 + o$X11 + o$X12 + o$X13 + o$X14 + o$X15 + o$X16 + o$X17
    + o$X18 + o$X19

    valueBox(
      value = as.integer(o$TOTAL[input$infectedtime]),
      subtitle = "Suceptible",
      icon = icon("user-alt")
    )
  })

  output$inf_ireland <- renderValueBox({
    o <- dataIreland()
    o$TOTAL <- o$X33 + o$X34 + o$X35 + o$X36 + o$X37 + o$X38 + o$X39 + o$X40 + o$X41
    + o$X42 + o$X43 + o$X44 + o$X45 + o$X46 + o$X47 + o$X48 + o$X49
    + o$X50 + o$X51 + o$X52 + o$X53 + o$X54 + o$X55 + o$X56 + o$X57 + o$X58
    + o$X58 + o$X59 + o$X60 + o$X61 + o$X62 + o$X63 + o$X64

    valueBox(
      as.integer(o$TOTAL[input$infectedtime]),
      "Infected",
      icon = icon("users"),
      color = "green"
    )
  })

  output$inf_france <- renderValueBox({
    o <- dataFrance()
    o$TOTAL <-  o$X19 + o$X20 + o$X21 + o$X22 + o$X23 + o$X24 + o$X25 + o$X26 + o$X27 + o$X28
    + o$X29 + o$X30 + o$X31 + o$X32 + o$X33 + o$X34 + o$X35 + o$X36

    valueBox(
      as.integer(o$TOTAL[input$infectedtime]),
      "Infected",
      icon = icon("users"),
      color = "green"
    )
  })

  output$inf_spain <- renderValueBox({
    o <- dataSpain()
    o$TOTAL <- o$X21 + o$X22 + o$X23 + o$X24 + o$X25 + o$X26 + o$X27 + o$X28
    + o$X29 + o$X30 + o$X31 + o$X32 + o$X33 + o$X34 + o$X35 + o$X36
    + o$X37 + o$X38 + o$X39 + o$X40

    valueBox(
      as.integer(o$TOTAL[input$infectedtime]),
      "Infected",
      icon = icon("users"),
      color = "green"
    )
  })

  output$inf_portugal <- renderValueBox({
    o <- dataPortugal()
    o$TOTAL <-  o$X19 + o$X20 + o$X21 + o$X22 + o$X23 + o$X24 + o$X25 + o$X26 + o$X27 + o$X28
    + o$X29 + o$X30 + o$X31 + o$X32 + o$X33 + o$X34 + o$X35 + o$X36

    valueBox(
      as.integer(o$TOTAL[input$infectedtime]),
      "Infected",
      icon = icon("users"),
      color = "green"
    )
  })

  output$time_ireland <- renderValueBox({
    valueBox(
      as.integer(dataIreland()$time[input$infectedtime]),
      "Duration",
      icon = icon("calendar-alt"),
      color = "yellow"
    )
  })

  output$time_france <- renderValueBox({
    valueBox(
      as.integer(dataFrance()$time[input$infectedtime]),
      "Duration",
      icon = icon("calendar-alt"),
      color = "yellow"
    )
  })

  output$time_spain <- renderValueBox({
    valueBox(
      as.integer(dataSpain()$time[input$infectedtime]),
      "Duration",
      icon = icon("calendar-alt"),
      color = "yellow"
    )
  })

  output$time_portugal <- renderValueBox({
    valueBox(
      as.integer(dataPortugal()$time[input$infectedtime]),
      "Duration",
      icon = icon("calendar-alt"),
      color = "yellow"
    )
  })

  output$rec_ireland <- renderValueBox({
    o <- dataIreland()
    o$TOTAL <- o$X65 + o$X66 + o$X67 + o$X68 + o$X69 + o$X70 + o$X71 + o$X72
    + o$X73 + o$X74 + o$X75 + o$X76 + o$X77 + o$X78 + o$X79 + o$X80
    + o$X81 + o$X82 + o$X83 + o$X84 + o$X85 + o$X86 + o$X87 + o$X88 + o$X89
    + o$X90 + o$X91 + o$X92 + o$X93 + o$X94 + o$X95 + o$X96

    valueBox(
      value = as.integer(as.integer(o$TOTAL[input$infectedtime])),
      "Recovered",
      icon = icon("procedures"),
      color = "orange"
    )
  })

  output$rec_france <- renderValueBox({
    o <- dataFrance()
    o$TOTAL <- o$X37 + o$X38 + o$X39 + o$X40 + o$X41 + o$X42 + o$X43 + o$X44 + o$X45
    + o$X46 + o$X48 + o$X49 + o$X50 + o$X51 + o$X53 + o$X54 + o$X55 + o$X56

    valueBox(
      value = as.integer(as.integer(o$TOTAL[input$infectedtime])),
      "Recovered",
      icon = icon("procedures"),
      color = "orange"
    )
  })

  output$rec_spain <- renderValueBox({
    o <- dataSpain()
    o$TOTAL <- o$X38 + o$X39 + o$X40 + o$X41 + o$X42 + o$X43 + o$X44 + o$X45
    + o$X46 + o$X48 + o$X49 + o$X50 + o$X51 + o$X53 + o$X54 + o$X55 + o$X56 + o$X57 + o$X58

    valueBox(
      value = as.integer(as.integer(o$TOTAL[input$infectedtime])),
      "Recovered",
      icon = icon("procedures"),
      color = "orange"
    )
  })

  output$rec_portugal <- renderValueBox({
    o <- dataPortugal()
    o$TOTAL <- o$X37 + o$X38 + o$X39 + o$X40 + o$X41 + o$X42 + o$X43 + o$X44 + o$X45
    + o$X46 + o$X48 + o$X49 + o$X50 + o$X51 + o$X53 + o$X54 + o$X55 + o$X56

    valueBox(
      value = as.integer(as.integer(o$TOTAL[input$infectedtime])),
      "Recovered",
      icon = icon("procedures"),
      color = "orange"
    )
  })

  observeEvent(input$p1Button, ({
    updateCollapse(session, "collapseExample", open = "Parameters")
    updateCollapse(session, "collapseExample", open = "Countermeasures")
  }))

  output$infectedmap <- renderLeaflet({

    leaflet() %>%
      addTiles() %>%
      setView(-7.77832031, 53.2734, 6) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })

  observe({
    req(input$infectedtime)

    if(grepl("Ireland",toString(input$countryc), fixed = TRUE)) {

      o <- dataIreland()
      ireland$infected[1] <- o$X33[input$infectedtime]
      ireland$infected[2] <- o$X34[input$infectedtime]
      ireland$infected[3] <- o$X35[input$infectedtime]
      ireland$infected[4] <- o$X36[input$infectedtime]
      ireland$infected[5] <- o$X37[input$infectedtime]
      ireland$infected[6] <- o$X38[input$infectedtime]
      ireland$infected[7] <- o$X39[input$infectedtime]
      ireland$infected[8] <- o$X40[input$infectedtime]
      ireland$infected[9] <- o$X41[input$infectedtime]
      ireland$infected[10] <- o$X42[input$infectedtime]
      ireland$infected[11] <- o$X43[input$infectedtime]
      ireland$infected[12] <- o$X44[input$infectedtime]
      ireland$infected[13] <- o$X45[input$infectedtime]
      ireland$infected[14] <- o$X46[input$infectedtime]
      ireland$infected[15] <- o$X47[input$infectedtime]
      ireland$infected[16] <- o$X48[input$infectedtime]
      ireland$infected[17] <- o$X49[input$infectedtime]
      ireland$infected[18] <- o$X50[input$infectedtime]
      ireland$infected[19] <- o$X51[input$infectedtime]
      ireland$infected[20] <- o$X52[input$infectedtime]
      ireland$infected[21] <- o$X53[input$infectedtime]
      ireland$infected[22] <- o$X54[input$infectedtime]
      ireland$infected[23] <- o$X55[input$infectedtime]
      ireland$infected[24] <- o$X56[input$infectedtime]
      ireland$infected[25] <- o$X57[input$infectedtime]
      ireland$infected[26] <- o$X58[input$infectedtime]
      ireland$infected[27] <- o$X59[input$infectedtime]
      ireland$infected[28] <- o$X60[input$infectedtime]
      ireland$infected[29] <- o$X61[input$infectedtime]
      ireland$infected[30] <- o$X62[input$infectedtime]
      ireland$infected[31] <- o$X63[input$infectedtime]
      ireland$infected[32] <- o$X64[input$infectedtime]

    }

    if(grepl("France",toString(input$countryc), fixed = TRUE)) {

      o <- dataFrance()
      france$infected[1] <- o$X19[input$infectedtime]
      france$infected[2] <- o$X20[input$infectedtime]
      france$infected[3] <- o$X21[input$infectedtime]
      france$infected[4] <- o$X22[input$infectedtime]
      france$infected[5] <- o$X23[input$infectedtime]
      france$infected[6] <- o$X24[input$infectedtime]
      france$infected[7] <- o$X25[input$infectedtime]
      france$infected[8] <- o$X26[input$infectedtime]
      france$infected[9] <- o$X27[input$infectedtime]
      france$infected[10] <- o$X28[input$infectedtime]
      france$infected[11] <- o$X29[input$infectedtime]
      france$infected[12] <- o$X30[input$infectedtime]
      france$infected[13] <- o$X31[input$infectedtime]
      france$infected[14] <- o$X32[input$infectedtime]
      france$infected[15] <- o$X33[input$infectedtime]
      france$infected[16] <- o$X34[input$infectedtime]
      france$infected[17] <- o$X35[input$infectedtime]
      france$infected[18] <- o$X36[input$infectedtime]

    }

    if(grepl("Portugal",toString(input$countryc), fixed = TRUE)) {

      o <- dataPortugal()
      portugal$infected[1] <- o$X19[input$infectedtime]
      portugal$infected[2] <- o$X20[input$infectedtime]
      portugal$infected[3] <- o$X21[input$infectedtime]
      portugal$infected[4] <- o$X22[input$infectedtime]
      portugal$infected[5] <- o$X23[input$infectedtime]
      portugal$infected[6] <- o$X24[input$infectedtime]
      portugal$infected[7] <- o$X25[input$infectedtime]
      portugal$infected[8] <- o$X26[input$infectedtime]
      portugal$infected[9] <- o$X27[input$infectedtime]
      portugal$infected[10] <- o$X28[input$infectedtime]
      portugal$infected[11] <- o$X29[input$infectedtime]
      portugal$infected[12] <- o$X30[input$infectedtime]
      portugal$infected[13] <- o$X31[input$infectedtime]
      portugal$infected[14] <- o$X32[input$infectedtime]
      portugal$infected[15] <- o$X33[input$infectedtime]
      portugal$infected[16] <- o$X34[input$infectedtime]
      portugal$infected[17] <- o$X35[input$infectedtime]
      portugal$infected[18] <- o$X36[input$infectedtime]

    }

    if(grepl("Spain",toString(input$countryc), fixed = TRUE)) {

      o <- dataSpain()
      spain$infected[1] <- o$X20[input$infectedtime]
      spain$infected[2] <- o$X21[input$infectedtime]
      spain$infected[3] <- o$X22[input$infectedtime]
      spain$infected[4] <- o$X23[input$infectedtime]
      spain$infected[5] <- o$X24[input$infectedtime]
      spain$infected[6] <- o$X25[input$infectedtime]
      spain$infected[7] <- o$X26[input$infectedtime]
      spain$infected[8] <- o$X27[input$infectedtime]
      spain$infected[9] <- o$X28[input$infectedtime]
      spain$infected[10] <- o$X29[input$infectedtime]
      spain$infected[11] <- o$X30[input$infectedtime]
      spain$infected[12] <- o$X31[input$infectedtime]
      spain$infected[13] <- o$X32[input$infectedtime]
      spain$infected[14] <- o$X33[input$infectedtime]
      spain$infected[15] <- o$X34[input$infectedtime]
      spain$infected[16] <- o$X35[input$infectedtime]
      spain$infected[17] <- o$X36[input$infectedtime]
      spain$infected[18] <- o$X36[input$infectedtime]
      spain$infected[19] <- o$X37[input$infectedtime]

    }

    labels <- sprintf(
      "<strong>%s</strong><br/>%d infected",
      ireland$name, as.integer(ireland$infected)
    ) %>% lapply(htmltools::HTML)

    labelsFr <- sprintf(
      "<strong>%s</strong><br/>%d infected",
      france$name, as.integer(france$infected)
    ) %>% lapply(htmltools::HTML)

    labelsPor <- sprintf(
      "<strong>%s</strong><br/>%d infected",
      portugal$name, as.integer(portugal$infected)
    ) %>% lapply(htmltools::HTML)

    labelsSp <- sprintf(
      "<strong>%s</strong><br/>%d infected",
      spain$name, as.integer(spain$infected)
    ) %>% lapply(htmltools::HTML)

    if(grepl("Ireland",toString(input$countryc), fixed = TRUE)) {
      leafletProxy("infectedmap") %>% addPolygons(data = ireland,fillColor = ~ palInfIre(infected),weight = 2,
                                                  opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
                                                  highlight = highlightopts, label = labels, labelOptions = labelopts)
    }

    if(grepl("France",toString(input$countryc), fixed = TRUE)) {
      leafletProxy("infectedmap") %>% addPolygons(data = france,fillColor = ~ palInfFr(infected),weight = 2,
                                                  opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
                                                  highlight = highlightopts, label = labelsFr, labelOptions = labelopts)
    }

    if(grepl("Portugal",toString(input$countryc), fixed = TRUE)) {
      leafletProxy("infectedmap") %>% addPolygons(data = portugal,fillColor = ~ palInfPor(infected),weight = 2,
                                                  opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
                                                  highlight = highlightopts, label = labelsPor, labelOptions = labelopts)
    }

    if(grepl("Spain",toString(input$countryc), fixed = TRUE)) {
      leafletProxy("infectedmap") %>% addPolygons(data = spain,fillColor = ~ palInfSp(infected),weight = 2,
                                                  opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
                                                  highlight = highlightopts, label = labelsSp, labelOptions = labelopts)
    }

  })

  # output$tablem <- renderTable({
  #   dataIreland()$CE_MATRIX
  #                                })

}

shinyApp(ui, server)
