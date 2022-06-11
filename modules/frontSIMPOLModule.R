source("modules/gaugeSIMPOLModule.R")

# module ui
frontSIMPOLModuleUI <- function(id){

  # set namespace via id
  ns <- NS(id)

#tags$div(id="test2", style="width:100%;height:"),c3Output(ns("test2"))
  tagList(

    # tags$head(
    #   tags$style(HTML("
    #
    #   .selectize-input {
    #     height: 40px;
    #     width: 300px;
    #     font-size: 12pt;
    #     padding-top: 5px;
    #   }
    #
    # "))
    # ),

    fluidRow(column(4,h3("SIMPOL Dashboard"),
    h4("Average Capacity across the catchment at different timescales",style="font-size:100%")),
    column(4,selectInput(ns('sectorInput'),label='Sector', choices = levels(as.factor(gaugedata$Sector)))),
    column(4,selectInput(ns('scenarioInput'),label='Model', choices = c("Baseline","2025","2045")))
    ),

    fluidRow(

      # column(3,h4("Overall Capacity", style = "text-align:center"),c3Output(ns("test2"))
      # ),
      column(3,gaugeSIMPOLModuleUI(ns("Gauge1"))),
      column(3,gaugeSIMPOLModuleUI(ns("Gauge2"))),
      column(3,gaugeSIMPOLModuleUI(ns("Gauge3"))),
      column(3,gaugeSIMPOLModuleUI(ns("Gauge4")))
      #column(3,gaugeSIMPOLModuleUI(ns("Gauge4")))
    ),

    br(),
    h4("Top 5 Spill Locations "),

    fluidRow(
          column(3,h4("Overall Spills", style = "text-align:center"),
                 c3Output(ns("PieProcess"))),
          column(3,h4("90 day Spills", style = "text-align:center"),
                  c3Output(ns("PieLabel"))),
          column(3,h4("60day Spills", style = "text-align:center"),
                 c3Output(ns("PieProduct"))),
          column(3, style = "overflow: hidden;",
                h4("Forecast Spills", style = "text-align:center"),
                  c3Output(ns("PieBranch")))
    #uiOutput(ns("GaugeAdditionalStats"))
    #piesModuleUI(ns("Pies"))


  ),

  fluidRow(
    br(),
    textOutput(ns('minDate')),
    h4("% Capacity over Time "),
    c3Output(ns('lineChart')),

  )


)


}

# module server
frontSIMPOLModule <- function(input, output, session){

# initialization ----
  ns <- session$ns
  # Generate Gauge Data
  createGaugeData <- function(data,timeperiod,sector){
    ###Function returns list of two dataframes, gaugeValues with name of sector and value and color
    ## additional with all four sectors,capacity and colors
      gaugeValues<-data%>%
      subset(TimePeriod==timeperiod)%>%
      subset(Sector==sector)%>%
      select(Sector,Capacity)%>%
      pivot_wider(names_from = Sector,values_from = Capacity)

      #Gauge color from string to conversion
      gaugeColor<-data%>%
      subset(TimePeriod==timeperiod)%>%
      subset(Sector==sector)%>%
      pull(Color)%>%
      col2hex(.)

      ##additional --- CHANGE COLORS BASED ON VALUE
      additional <-data %>%
      subset(TimePeriod==timeperiod)%>%
      mutate(gColor = if_else(Capacity < 15,'DarkGreen',if_else(Capacity < 40,'Green',if_else(Capacity < 50,'Yellow',
                                                                                   if_else(Capacity < 80,'Orange','Red')))))

      gdata <- list(gaugeValues,gaugeColor,additional)
      names(gdata) <- c('values','color','additional')

      return (gdata)

  }

  ##Reacts when input is changed which updates the dashboard
  gdata <- reactiveValues(data=gaugedata)


  ##Observer waits for sector$Input to be changed and updates the chart
  observeEvent(input$sectorInput,{
    gdata$Gauge1Data <- createGaugeData(gaugedata,'Overall',input$sectorInput)
    gdata$Gauge2Data <- createGaugeData(gaugedata,'Last90Days',input$sectorInput)
    gdata$Gauge3Data <- createGaugeData(gaugedata,'Last30Days',input$sectorInput)
    gdata$Gauge4Data <- createGaugeData(gaugedata,'Forecast5days',input$sectorInput)

  })



  ##When changed recalls module with updated data
  observeEvent(input$sectorInput,{
    Gauge1<- callModule(gaugeSIMPOLModule, "Gauge1",title="Overall",gdata$Gauge1Data)
    Gauge2<- callModule(gaugeSIMPOLModule, "Gauge2",title="Last 90 days",gdata$Gauge2Data)
    Gauge3<- callModule(gaugeSIMPOLModule, "Gauge3",title="Last 60 days",gdata$Gauge3Data)
    Gauge4<- callModule(gaugeSIMPOLModule, "Gauge4",title="Forecast 7 days",gdata$Gauge4Data)

  })



  ###RENDER PIES ##############3
  output$PieProcess <-  renderC3({pieData %>%
                                  c3()%>%
                                  c3_pie()})

  output$PieLabel <-  renderC3({pieData %>%
                                  c3()%>%
                                  c3_pie()})

 output$PieProduct <-  renderC3({pieData %>%
                                  c3()%>%
                                  c3_pie()})

 output$PieBranch <-  renderC3({pieData %>%
                                c3()%>%
                                c3_pie()})


  #####RENDER Line CHART ON BOTTOm
  output$lineChart <- renderC3({
    capacityTS%>%
    pivot_wider(names_from='Sector',values_from='dailyAve')%>%
    c3(x = 'day')%>%
    yAxis(label='% Capacity Used')%>%
    subchart()
  })


  brush <- reactive({

    input$lineChart

      if(!is.null(input$lineChart)){


        output$minDate <- renderText({input$lineChart[1]})
        output$maxDate <- input$lineChart[2]

        # retBrush    <- lstData$DD.Signaleringen$detectiedatum >= minDate &
        #                lstData$DD.Signaleringen$detectiedatum <= maxDate

      }else{
        retBrush <- TRUE
      }

      return(retBrush)
  })

}
