  source("modules/gaugeModule.R")
# module ui
forecastModuleUI <- function(id){


  # set namespace via id
  ns <- NS(id)

  tagList(
    setShadow(id=ns('spillGraph')),
    #h3("Maps"),
     # shinydashboard::box((tags$style(type = "text/css", "#buildmapdemo {height: calc(100vh - 80px) !important;}"),width = 1200, solidHeader = TRUE, leafletOutput(ns('buildmapdemo'))))
     fluidRow(class = "row1",
       column(12,div(style="height:400px;",leafletOutput(ns('buildmapdemo'),height=350)))
     ),
     #column(width=12,box(width = 1200, solidHeader = TRUE,leafletOutput('buildmapdemo',height = 300)))
     #fluidRow(tags$iframe(seamless = "seamless", src = "https://forecast.io/embed/#lat=51.5541016&lon=-1.7823604&name=London", height = 300, width = 1600))
     fluidRow(class = "row2",collapsible = FALSE, solidHeader = FALSE,height=300,
       column(6,includeHTML("widget.html")),
       column(6, div(style = "height:250px;",dygraphOutput(ns('spillGraph'),height=250),depth=2))
   ),
   tags$head(tags$style("
   .row1{height:400px;}"
  ))
 )

}



####FUNCTION PROCESS SPILLS
processSpills <- function(spills){
    ## Convert Start to datetime then create enddate by adding duration of event to start
    spills$StartDate <- as.POSIXct(spills$StartDate,format='%d/%m/%Y %H:%M')
    spills$EndDate <- spills$StartDate + hours(spills$EventDur)
    spills$start <- spills$StartDate
    spills$end <- spills$EndDate

    ##Trim SSC field,Capitalise and join, Could do this in SQL , probably refactor
    spills$joinSSC <- trimws(toupper(spills$ModName))
    outfalls$joinSSC <- trimws(toupper(outfalls$SSC))
    ##join regatta to spill freqeuncy
    spillsTime<- merge(spills,outfalls,by.x='joinSSC',by.y='joinSSC')
    spillsTimeSeries <- spillsTime[,c('coords.x2','coords.x1','start','end','OutletName','ModName','joinSSC','EventNo','EventDur','StartDate','EndDate','EventQty')]
    colnames(spillsTimeSeries)[1:2] <-c("Latitude","Longitude")
    demo <- spillsTimeSeries[,c('Latitude','Longitude','start','end','OutletName','ModName','EventQty','EventDur')]
    demo$id<-seq.int(nrow(demo))
    demo <- data.frame(demo)
    demo<- demo[order(demo$start,demo$end),]

    return (demo)
}


##### ATTEMPT TO INJECT JAVASCRIPT
registerPlugin <- function(map, plugin) {
      map$dependencies <- c(map$dependencies, list(plugin))
      map
    }

esriPlugin <- htmlDependency("leaflet-icon-pulse",version = "1.0",
                src = "C:/Users/ThinkPad/Desktop/alpha/Phantom/Serpent/Basilisk/app/www/",
                script = "L.Icon.Pulse.js",stylesheet ="L.Icon.Pulse.css")



iconSet = pulseIconList(
  red = makePulseIcon(color = "#ff0000"),
  blue = makePulseIcon(color = "#0000ff")
)



forecastModule <- function(input, output, session, mapData){

  # get namespace based on session
  ns <- session$ns

  ### Make sure the proper help is loaded.
  #source("modules/helpLogic.R",local=TRUE)
  ##first map
    output$buildmapdemo <-  renderLeaflet({
    csomap <- leaflet(geojsonio::geojson_json(demo[1950:2013,]))%>%
    addTiles(group = 'OSM') %>%
    registerPlugin(esriPlugin)%>%
    addProviderTiles('OpenWeatherMap.Precipitation',
        options=providerTileOptions(showLegend=FALSE,opacity=0.7,apiKey="ecdad19882a76053af59be98467e3c4a"),group='Radar')%>%
    # addOpenweatherTiles(apikey="ecdad19882a76053af59be98467e3c4a",layers='precipitationClassic',group='Rain',opacity=0.4)%>%
    addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',group='CartoBlack')%>%
    #addTiles('http://maps.openweathermap.org/maps/2.0/weather/PAO/{z}/{x}/{y}?date=1603238400&opacity=0.9&fill_bound=true&appid=3161dfe75ae578bb10796fb82bd51df2',group='OWM2')%>%
    addTiles('http://maps.openweathermap.org/maps/2.0/relief/TA2/{z}/{x}/{y}?palette=0:FF0000;1000:00FF00;3000:0000FF&appid=74494e7710eb3ccc59e9df783c80805e',group='OWM2')%>%
    setView( -0.1260935, 51.5068896, zoom = 9) %>%
    addProviderTiles('Esri.WorldStreetMap', group = 'Esri') %>%
    addProviderTiles('CartoDB.Positron', group = 'CartoDB') %>%
    addPolygons(data=SSC,layerId=~subcatchme,stroke = T, smoothFactor = 0.05, fillOpacity = 0.5,fillColor =~colorfact(user_text1),color='black', weight = 1,popup= ~user_text1,
    highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE)) %>%
    addCircleMarkers(data=regatta_,weight=1,layerId=regatta_$OutletName,color='black',radius=5,fillCol='yellow',popup=paste("<b>","Outfall Name:",regatta_$OutletName,"</b>","<br>",
     "Ave Spill Freq/year:", regatta_$`Ave Spill freq/yr`, "<br>",
     "Ave Spill Vol:",regatta_$`Ave spill vol/yr`, "<br>"),
    fillOpacity=1,group='CSOs', label =~SSC.y,labelOptions = labelOptions(noHide = F))%>%
    addTimeline(
      timelineOpts = timelineOptions(
        pointToLayer = htmlwidgets::JS(
          "
          function(data, latlng) {
            var icon = L.icon.pulse({iconSize:[10,10],color:'red'});
            return L.marker(latlng, {icon:icon}
              ).bindTooltip(
              // data.properties will have the columns from sf in R
              'Name: ' + data.properties.OutletName + '<br/>Duration: ' + data.properties.EventDur +
              '<br/>Qty: ' + data.properties.EventQty,
              {permanent: false}
            ).openTooltip()
          }
          "
        )
      ),
        sliderOpts=sliderOptions(duration=80000,position="bottomleft"),
        group = 'Pulse'
    )%>%
    addLayersControl(
       baseGroups = c('Esri', 'CartoDB','CartoBlack','Rain'),
       overlayGroups = c('CSOs','Radar','Pulse','OWM2'),
       options = layersControlOptions(collapsed = TRUE)
     )
  })

  #targetSSC <- reactiveVal(value='SSC_HorseFerryRd')
  observe({
      click<-input$buildmapdemo_marker_click
      if(is.null(click))
        return()

      targetSSC <-regatta_%>%
      filter(OutletName==click$id)%>%
      pull(SSC.x)

      output$spillGraph <-renderDygraph({
        timeRiver%>%
        filter(ModName==targetSSC)%>%
        pivot_wider(names_from='ModName',values_from='value')%>%
        read.zoo(.)%>%
        dygraph(.,main='Spill Timeseries')%>%
        dyShading(from = "2015-01-06", to = "2015-02-10",color = "#FFE6E6")
      })

      print(targetSSC)
    })

    ####DYGRAPH IN BOTTOM RIGHT HAND CORNER -- SHOWING SPILL TIME SERIES
    # Graph controlled by clicks on spill map and --- time
    # output$spillGraph <-renderDygraph({
    #   timeRiver%>%
    #   filter(ModName=="SSC_HorseFerryRd")%>%
    #   pivot_wider(names_from='ModName',values_from='value')%>%
    #   read.zoo(.)%>%
    #   dygraph(.,main='Spill Timeseries')%>%
    #   dyShading(from = "2015-01-06", to = "2015-02-10",color = "#FFE6E6")
    # })




}
