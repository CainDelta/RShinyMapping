# module ui
analysisModuleUI <- function(id,stats){

  # set namespace via id
  ns <- NS(id)


  tagList(
    #h3("Maps"),
     # shinydashboard::box((tags$style(type = "text/css", "#buildmapdemo {height: calc(100vh - 80px) !important;}"),width = 1200, solidHeader = TRUE, leafletOutput(ns('buildmapdemo'))))


     leafletOutput(ns('halfmap'),height=350),
     absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
        draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
        width = 300, height = "auto",

        h2("Scenario Explorer"),
        selectInput(ns('variableInput'), label='Variable', choices = levels(as.factor(stats$Variable))), ###dropdown list of module names, very useful for filtering
        selectInput(ns('statsInput'),label='Statistics',choices =levels(as.factor(stats$Statistics))), ##change to max
        selectInput(ns('modInput'),label='Module',choices =levels(as.factor(SSCResults$ModName)),selected='SSC_BATHROAD_SSO'), ##change to max
        checkboxInput(ns("slegend"), "Show legend", TRUE),
        selectInput(ns("scolors"), "Color Scheme",rownames(subset(brewer.pal.info, category %in% c("seq", "div"))))
        ),

    dygraphOutput(ns("resultsDy"),height=300),

     tags$head(
           # Include our custom CSS
           includeCSS("styles.css")
           #includeScript("gomap.js")
         )

  )

}

# module server
analysisModule <- function(input, output, session, mapData,stats){

  # get namespace based on session
  ns <- session$ns

  ##Load Stats
  v <- reactiveValues(data = stats)



  ################MOGDEN MAP ########################
  # Standard map of Mogden catchment which uses two reactive functions
  # 1. Observer 1 function , changees the data based on the drop down input
  # 2. Observer 2 function creates a legend when the input is changed
  # 3. Data used -- STATS -- this is the prep peraeted table with the original baseline 2016 and one scenario 2045
  # 4. This table is in original sqlite database -- will need updating when comparing new scenarios
  #
  ######################################################
  #################LOAD BOD REACTIVE
  observeEvent({input$statsInput
                input$variableInput
                1},{
            v$data <- stats%>%
            subset(Statistics == input$statsInput)%>%
            subset(Variable == input$variableInput)%>%
            pivot_wider(.,id_cols =c(ModName,Variable), names_from = c(Model,Year),values_from = Value)%>%
            mutate(Change_Baseline = (Baseline_2045/Baseline_2016-1)*100 ) %>%
            mutate(Change_Scenario1 = (Scenario1_2045/Baseline_2016-1)*100 )%>%
            merge(mapData,., by.x='nodeinfone',by.y='ModName')

            })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
    # colorpal <- reactive({data = Load()
    #   colorBin(input$colors, data$Change_Baseline)})

    observeEvent({input$statsInput
                  input$slegend
                  input$variableInput
                  input$scolors
                  1},
         {v$colorpal <-colorBin(input$scolors, v$data$Change_Baseline)})


  output$halfmap<- renderLeaflet({
        ldn<-leaflet()%>%
        addTiles(group = 'OSM') %>%
        addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',group='CartoBlack')%>%
        addProviderTiles('Esri.WorldStreetMap', group = 'Esri') %>%
        addProviderTiles('CartoDB.Positron', group = 'CartoDB') %>%
        setView(-0.11, 51.5, 9)%>%
        addLayersControl(baseGroups = c('OSM', 'Esri', 'CartoDB','CartoBlack'),
                       options = layersControlOptions(collapsed = FALSE))
    })

    ###COLOR CHANGES
    # bins <- c(-100,0,10,100)
    # pal <- colorBin(c("green", "yellow","red"), domain = bod$Change_Baseline, bins = 4)

    observeEvent({input$statsInput
                  input$variableInput
                  input$scolors
                  1},{
          pal <- v$colorpal
          #pal <- colorBin(c("green", "yellow","red"),domain=data$Change_Scenario1)
          title = str(input$statsInput)
          leafletProxy("halfmap",data = v$data)%>%
               clearShapes() %>%
               addPolygons(stroke = T, smoothFactor = 0.05, fillOpacity = 0.8,fillColor=~pal(Change_Baseline),color='black',
                           weight = 1,label =~nodeinfone,labelOptions = labelOptions(noHide = F),
                          highlight = highlightOptions(weight= 2,color = "black"))
            })


    #Use a separate observer to recreate the legend as needed.
    observeEvent({input$slegend
             input$statsInput
             input$variableInput
             input$scolors
                  1},{
        pal <- v$colorpal
        proxyLeaf <- leafletProxy("halfmap", data = v$data)
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxyLeaf %>% clearControls()
        if (input$slegend) {
          proxyLeaf %>% leaflet::addLegend(position = "bottomleft",
                    pal = pal, values = ~Change_Baseline,title = '% Changes Baseline 2016 -> 2045'
                  )
              }
          })

    observeEvent({input$variableInput
                  input$modInput
                  1},{

    output$resultsDy <- renderDygraph({ ##function controlled by inputs, dygraph may not be way to go but good first start

            ##DYGRAPH
              label = input$variableInput
              main = paste('Daily Average ',input$modInput, '',input$variableInput)
              SSCResults%>%
              subset(VarName == input$variableInput)%>%
              select(`Date/Time`,ModName,value)%>%
              subset(ModName== input$modInput)%>%
              # filter(`Date/Time` >= input$dateRange[1] & `Date/Time` <= input$dateRange[2])%>%
              pivot_wider(names_from='ModName',values_from='value')%>%
              read.zoo(.)%>%
              dygraph(.,ylab=label)%>%
              dyRangeSelector()
              #dyRoller(rollPeriod = 24)%>%
              #dyOptions(fillGraph=TRUE,fillAlpha=0.4)

              # dyHighlight(highlightCircleSize = 5,
              #             highlightSeriesBackgroundAlpha = 0.2,
              #             hideOnMouseOut = FALSE,
              #             highlightSeriesOpts = list(strokeWidth = 3)) %>%

            })
        })


}
