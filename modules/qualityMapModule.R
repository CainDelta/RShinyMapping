# module ui
qualityMapModuleUI <- function(id,stats){

  # set namespace via id
  ns <- NS(id)


  tagList(
    #h3("Maps"),
     # shinydashboard::box((tags$style(type = "text/css", "#buildmapdemo {height: calc(100vh - 80px) !important;}"),width = 1200, solidHeader = TRUE, leafletOutput(ns('buildmapdemo'))))


     leafletOutput(ns('fullmap'),height=650),

     absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
        draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Scenario Explorer"),
        selectInput(ns('varInput'), label='Variable', choices = levels(as.factor(stats$Variable))), ###dropdown list of module names, very useful for filtering
        selectInput(ns('statInput'),label='Statistics',choices =levels(as.factor(stats$Statistics))), ##change to max
        checkboxInput(ns("legend"), "Show legend", TRUE),
        selectInput(ns("colors"), "Color Scheme",rownames(subset(brewer.pal.info, category %in% c("seq", "div"))))
        ),

     tags$head(
           # Include our custom CSS
           includeCSS("styles.css")
           #includeScript("gomap.js")
         )

  )

}

# module server
qualityMapModule <- function(input, output, session, mapData,stats){

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
  observeEvent({input$statInput
                input$varInput
                1},{
            v$data <- stats%>%
            subset(Statistics == input$statInput)%>%
            subset(Variable == input$varInput)%>%
            pivot_wider(.,id_cols =c(ModName,Variable), names_from = c(Model,Year),values_from = Value)%>%
            mutate(Change_Baseline = (Baseline_2045/Baseline_2016-1)*100 ) %>%
            mutate(Change_Scenario1 = (Scenario1_2045/Baseline_2016-1)*100 )%>%
            merge(mapData,., by.x='nodeinfone',by.y='ModName')

            })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
    # colorpal <- reactive({data = Load()
    #   colorBin(input$colors, data$Change_Baseline)})

    observeEvent({input$statInput
                  input$legend
                  input$varInput
                  input$colors
                  1},
         {v$colorpal <-colorBin(input$colors, v$data$Change_Baseline)})


  output$fullmap<- renderLeaflet({
        mog<-leaflet()%>%
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

    observeEvent({input$statInput
                  input$varInput
                  input$colors
                  1},{
          pal <- v$colorpal
          #pal <- colorBin(c("green", "yellow","red"),domain=data$Change_Scenario1)
          title = str(input$statInput)
          leafletProxy("fullmap",data = v$data)%>%
               clearShapes() %>%
               addPolygons(stroke = T, smoothFactor = 0.05, fillOpacity = 0.8,fillColor=~pal(Change_Baseline),color='black',
                           weight = 1,label =~nodeinfone,labelOptions = labelOptions(noHide = F),
                          highlight = highlightOptions(weight= 2,color = "black"))
            })


    #Use a separate observer to recreate the legend as needed.
    observeEvent({input$legend
             input$statInput
             input$varInput
             input$colors
                  1},{
        pal <- v$colorpal
        proxy <- leafletProxy("fullmap", data = v$data)
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        if (input$legend) {
          proxy %>% leaflet::addLegend(position = "bottomleft",
                    pal = pal, values = ~Change_Baseline,title = '% Changes Baseline 2016 -> 2045'
                  )
              }
          })

}
