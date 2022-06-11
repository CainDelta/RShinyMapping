# module ui
mapModuleUI <- function(id){

  # set namespace via id
  ns <- NS(id)

  tagList(
    #h3("Maps"),
     # shinydashboard::box((tags$style(type = "text/css", "#buildmapdemo {height: calc(100vh - 80px) !important;}"),width = 1200, solidHeader = TRUE, leafletOutput(ns('buildmapdemo'))))
     leafletOutput(ns('buildmapdemo'),height=650) 

  )

}

# module server
mapModule <- function(input, output, session, mapData){

  # get namespace based on session
  ns <- session$ns



  ### Make sure the proper help is loaded.
  #source("modules/helpLogic.R",local=TRUE)
  ##first map
  output$buildmapdemo <-  renderLeaflet({
    csomap <- leaflet(mapData)%>%
    addTiles(group = 'OSM') %>%
    addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',group='CartoBlack')%>%
    addProviderTiles('Esri.WorldStreetMap', group = 'Esri') %>%
    addProviderTiles('CartoDB.Positron', group = 'CartoDB') %>%
    addPolygons(layerId=~subcatchme,stroke = T, smoothFactor = 0.05, fillOpacity = 0.5,fillColor =~colorfact(user_text1),color='black', weight = 1,popup= ~user_text1,
    highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE)) %>%
    addLayersControl(
       baseGroups = c('OSM', 'Esri', 'CartoDB','CartoBlack'),
       options = layersControlOptions(collapsed = FALSE)
     )
    return(csomap)
  })


}
