# module ui
spillMapModuleUI <- function(id){

  # set namespace via id
  ns <- NS(id)

  tagList(
    #h3("Maps"),
     # shinydashboard::box((tags$style(type = "text/css", "#buildmapdemo {height: calc(100vh - 80px) !important;}"),width = 1200, solidHeader = TRUE, leafletOutput(ns('buildmapdemo'))))
     leafletOutput(ns('ldnmap'),height=650)

  )

}

# module server
spillMapModule <- function(input, output, session, mapData){

  # get namespace based on session
  ns <- session$ns

  ### Make sure the proper help is loaded.
  #source("modules/helpLogic.R",local=TRUE)
  output$ldnmap <-  renderLeaflet({
  ldn <- leaflet(ldnbound)%>%
  addTiles(group = 'OSM') %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',group='CartoBlack')%>%
  addProviderTiles('Esri.WorldStreetMap', group = 'Esri') %>%
  addProviderTiles('CartoDB.Positron', group = 'CartoDB') %>%
  addMapPane("CSOs", zIndex = 450) %>%
  addMapPane("SSCs", zIndex = 420) %>%
  addPolygons(stroke = T, smoothFactor = 0.05, fillOpacity = 0.35,fillColor='transparent',color='black', weight = 2,group='Boundary')%>%
  addCircleMarkers(data=regatta_,weight=3,color='black',radius=10,fillCol='yellow',popup=paste("<b>","Outfall Name:",regatta_$OutletName,"</b>","<br>",
   "Ave Spill Freq/year:", regatta_$`Ave Spill freq/yr`, "<br>",
   "Ave Spill Vol:",regatta_$`Ave spill vol/yr`, "<br>"),
  fillOpacity=1,group='CSOs', label =~SSC.y,labelOptions = labelOptions(noHide = F))%>%
  # addCircleMarkers(data=regatta_,weight=3,color='black',radius=10,fillCol='yellow',popup=paste("<b>","Outfall Name:",regatta_$OutletName,"</b>","<br>",
  #  "Ave Spill Freq/year:", regatta_$`Ave Spill freq/yr`, "<br>",
  #  "Ave Spill Vol:",regatta_$`Ave spill vol/yr`, "<br>"),
  # fillOpacity=1,group='Spill Frequency', label =~SSC.y,labelOptions = labelOptions(noHide = F))%>%
  #addCircleMarkers(data=outfalls,weight=3,color='black',radius=5,fillCol='red',fillOpacity=1,group='Outfalls',label =~SSC_1,labelOptions = labelOptions(noHide = F))%>%
  addPolygons(data=icm,stroke = T, smoothFactor = 0.05, fillOpacity = 0.6,fillColor=~icm_color(Name),color='black', weight = 0.5,group='SSCs',
              label =~Name,labelOptions = labelOptions(noHide = F))%>%
  #addSearchFeatures(targetGroups = 'SSCs', options = searchFeaturesOptions(zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,autoCollapse = TRUE, hideMarkerOnCollapse = TRUE)) %>%
  addLayersControl(
     baseGroups = c('OSM', 'Esri', 'CartoDB','CartoBlack'),
     overlayGroups = c("CSOs", "Boundary",'SSCs',"Spill Frequency"),
     options = layersControlOptions(collapsed = TRUE))
})


}
