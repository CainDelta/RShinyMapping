
uploadDataModuleUI <- function(id){
  # set namespace via id
  ns <- NS(id)
  # Create a namespace function using the provided id

 #begin UI (wrap all input/ouput in ns() call)
 tagList(
   #

  # App title ----
  h3("Uploading Files"),

   fluidRow(column(3,
  # Input: Select a file ----
    fileInput(ns("file1"), "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".txt",
                       ".csv")),
    # Horizontal line ----
    tags$hr(),

    # Input: Checkbox if file has header ----
    checkboxInput(ns("header"), "Header", TRUE),

    # Input: Select separator ----
    radioButtons(ns("sep"), "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),

    # Input: Select quotes ----
    radioButtons(ns("quote"), "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"'),

    # Input : Check Names ---
    radioButtons(ns("checknames"),"CheckNames",
              choices = c("False" = FALSE,
                           "True" = TRUE),
              select = FALSE ),

    # Input: Select number of rows to display ----
      radioButtons(ns("disp"), "Display",
                    choices = c(Head = "head",
                                All = "all"),
                           selected = "head"),

    # Horizontal line ----
    tags$hr(),


    # shinyFilesButton(
    #                id=ns("get_file_path"),
    #                label="Click Here to Select",
    #                title="Select a file",
    #                multiple= FALSE,
    #                buttonType = "default",
    #                class = NULL),
  shinyFilesButton(ns("file"), "File select", "Please select a file", multiple = TRUE, viewtype = "detail"),


),
 column(8,  # Output: Data file ----
    tableOutput(ns('contents')),
    verbatimTextOutput(ns("filepaths")))

)

 )
}



uploadDataModule <- function(input, output, session){

  # get namespace based on session
  ns <- session$ns

  ##reactive func - this is whats called everytime
  values <- reactive({df == NULL})


  ### SHINY FILES BUTTON -- NO LIMIT
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "file", roots = volumes, session = session)
# by setting `allowDirCreate = FALSE` a user will not be able to create a new directory
  shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"))
  shinyFileSave(input, "save", roots = volumes, session = session, restrictions = system.file(package = "base"))

    ##Event checks file and prints file name?
    observeEvent(input$file1,{
      output$filename <- renderText({
        input$file1$datapath
      })

    })

    ###FILE CHOOSE --5MB LIMIT
    observeEvent({input$file1
                 input$header
                 input$checknames
                 1},{
      output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      df <-read.csv(inFile$datapath,
                  header = input$header,
                  sep = input$sep,
                  quote = input$quote,
                  check.names = input$checknames)

      ##Function returns head or full dataframe
      if(input$disp == "head") {
          return(head(df))  }
        else {
            return(df)
                }

        })


    })

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



  }
