####REWRITE FORECAST FUNCITON
source("modules/customWeather.R")

dlForecastUI <- function (id){
  ns <- NS(id)
  # Create a namespace function using the provided id

 #begin UI (wrap all input/ouput in ns() call)
 tagList(
   # App title ----
   h3("Download Forecast data"),
   column(3,
     # Input: Choose dataset ----
    selectInput(ns("dataset"), "Choose a dataset:",
                  choices = c("Week 20/10/2020")),
     # Button
     downloadButton(ns("downloadData"), "Download")

   ),
   column(9,
     tableOutput(ns("table"))
   )
 )
}

dlForecastModule <- function(input, output, session){

  # get namespace based on session
  ns <- session$ns

  # Reactive value for selected dataset ----
  datasets <- reactive({df==NULL})

observeEvent(input$dataset,{

  # datasetInput <- switch(input$dataset,
  #                        "rock" = rock,
  #                        "pressure" = pressure,
  #                        "cars" = cars)

  rain <- get_hourly_Weather("London")
  datasetInput <- exportSIMPOLRain(rain)

  output$table <- renderTable({
                           datasetInput
                         })


# Downloadable csv of selected dataset ----
 output$downloadData <- downloadHandler(
        filename = function() {
        paste(input$dataset, ".csv", sep = "")
                           },
        content = function(file) {
        write.csv(datasetInput, file, row.names = FALSE)
                           })

})

}
