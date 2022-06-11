
### FUNCTION TO UPLOAD DATA
uploadSpillData <- function(connection,runName,data){
  dbWriteTable(conn=connection, name=runName, value=data,overwrite=TRUE) #append = TRUE)
}

uploadDataModuleUI <- function(id){

# set namespace via id
  ns <- NS(id)

  tagList(

    column(4,
    #fileInput(ns("file"), NULL, accept = c(".csv", ".txt")),
    br(),

    shinyFilesButton(ns("file"), "File select", "Please select a file", multiple = TRUE, viewtype = "detail"),
    br(),
    br(),
    numericInput(ns("n"), "Skiplines:", min = 0, max = 100, value = 0),
    textInput(ns("caption"), "Run Name", "Default"),
    br(),
    selectInput(ns('DataType'),'Select Type',c('Quality','Spills','Capacity','Default'),selected = 'Default',selectize = TRUE),
    br(),
    actionButton(ns("goButton"), "Select"),
    actionButton(ns("uploadButton"), "Upload")),
    column(8,
    p("Click Select to display data and Upload to load into databse."),
    verbatimTextOutput(ns("filepaths")),
    verbatimTextOutput(ns('dbiResult')),
    DT::dataTableOutput(ns("head"))
    )

    )


}

uploadDataModule <- function(input, output, session) {

  con <- dbConnect(RSQLite::SQLite(), "../app/data/SIMPOL.sqlite")
  # get namespace based on session
  ns <- session$ns


  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "file", roots = volumes, session = session)
  # by setting `allowDirCreate = FALSE` a user will not be able to create a new directory
  shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"))
  shinyFileSave(input, "save", roots = volumes, session = session, restrictions = system.file(package = "base"))

  ## print to browser
  observeEvent(input$file,{
    output$filepaths <- renderPrint({
        if (is.integer(input$file)) {
          cat("No files have been selected (shinyFileChoose)")
        } else {
          parseFilePaths(volumes, input$file)
        }
      })

  })


  # builds a reactive expression that only invalidates
 # when the value of input$goButton becomes out of date
 # (i.e., when the button is pressed)
   ntext <- eventReactive(input$goButton, {
     input$n
   })

   ###Select data type being uploaded
   dataType<- eventReactive(input$goButton, {
     input$DataType
   })

   runName <- eventReactive(input$uploadButton, {
     input$caption
   })

   output$nText <- renderText({
     ntext()
   })


   ###UPLOAD function , reactive, when the Select button is clicked the data is loaded into a variable
   # 1. Implementing a select input for the different types of table options

   data <- eventReactive(input$goButton
                          ,{
    req(input$file)
    inFile <- parseFilePaths(volumes, input$file)



    ##Convert table to SIMPOL Processed
    if(input$DataType=='Quality'){
      ##If Quality is chosen then file is converted
      calProcess(read.csv(inFile$datapath,check.names=FALSE,skip=input$n))
    }else {
      read.csv(inFile$datapath,check.names=FALSE,skip=input$n)
    }


  })


  upload <-eventReactive(input$uploadButton,{
    req(input$file)
    rn <- runName()
    uploadSpillData(con,rn,data())
    rm(data) ###Delete loaded data file ??
  })

  output$RunName <- renderText({
    runName()
  })

  output$head <- DT::renderDataTable({
    head(data(), 30)
  })

  output$dbiResult <-renderText({
    upload()
  })


}
