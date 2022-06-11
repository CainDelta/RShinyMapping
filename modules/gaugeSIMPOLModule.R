###
### The gauge module renders a gauge with a title and statistics below the gauge
###
gaugeSIMPOLModuleUI <- function(id){

  # set namespace via id
  ns <- NS(id)

  tagList(uiOutput(ns("GaugeWithStats")))

}

gaugeSIMPOLModule <- function(input,output,session,title,simGaugeData){

  # get namespace based on session
  ns <- session$ns

  ### Render percentages underneath gauges-----
  output$GaugeAdditionalStats<- renderUI({

    lstData <- simGaugeData$additional

    values <- lstData

    ShowColorInfo(values)
  })

  ### Render gauges ----
  output$Gauge <- renderC3({
    #Data  <- simGaugeData
    simGaugeData$values%>%
    c3()%>%
    c3_gauge(width=20,height=200,pattern=c("#238823","#FFBF00","#FF8C00","#D2222D"),
            list(unit = "value", max = 100, values = c(1,15,40,60, 85)))
    ###values are for changing color depending on severity
  })


  output$GaugeWithStats <- renderUI({

    L <- list(div( h4(title, style = "text-align:center"),
                   c3Output(ns('Gauge')), style = "width:100%; margin: auto",
                   uiOutput(ns("GaugeAdditionalStats"))))
    return(L)

  })
}

#' dashboard front panel helper function
#'
#' @description Creates ui component to which shows color information underneath gauges
ShowColorInfo <- function(values){
  nColor <- length(colors)

  colors <- values$gColor

  displayColors <- colors
  #sectors <- list("NETWORK","TUNNEL","CROSSNESS","BECTON")
  sectors <- values$Sector

  values <- values$Capacity
  nColor <- length(colors)

  L <- fluidRow(style = "text-align: center",
                lapply(1:nColor, function(i){
                  column(3,
                         div(style = "color: 'black';font-size:80%",sectors[i]),
                         h1("-",style = paste("font-size:300%; margin:1px; line-height:3px; color:",displayColors[i])),
                         h4(style="font-size:80%",values[i])
                  )
                })
  )
  return(L)
}
