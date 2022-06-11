rm(list=ls())

# public R libraries
library(shiny)
library(shinydashboard)
library(googleVis)
library(plyr)
library(dplyr)
library(reshape2)
library(DT)
library(RColorBrewer)
library(htmltools)
library(htmlwidgets)
library(leaflet.extras)
library(leaflet.extras2)
library(c3)
library(httr)
library(owmr)
#
#
library(leaftime)
library(leaflet)
library(rgdal)
library(viridis)
library(ECharts2Shiny)
library(dygraphs)
library(tidyverse)
library(xts)
library(shinymaterial)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyFiles)
library(fs)
library(DBI)
library(RSQLite)
library(gplots)

# custom R libraries
library(FrissC3Charts)
library(FrissMessageBox)
library(FrissIntroJS)
library(FrissSwitch)
library(FrissNotie)

# module definitions
source("modules/filterModule.R")
source("modules/piesModule.R")
source("modules/frontPanelModule.R")
source("modules/flowChartModule.R")
source("modules/hitsModule.R")
source("modules/mapModule.R")
source("modules/qualityMapModule.R")
source("modules/analysisModule.R")
source("modules/spillMapModule.R")
source("modules/forecastModule.R")
source("modules/uploadDataModule.R")
source("modules/frontSIMPOLModule.R")
source("modules/downloadForecastModule.R")

# helper routines
source("helpers.r")

# temp header
FrissHeader <- list(

  tags$a(href = "http://www.friss.eu/en", tags$img(src="friss_small2.svg", id = "FrissLogo")),

  singleton(includeScript("www/underscore.js")),
  singleton(includeScript("www/jquery-ui.js")),
  singleton(includeScript("www/shinyState.js")),
  singleton(includeScript("www/d3.js")),
  singleton(includeCSS("www/friss.css")),
  singleton(includeCSS("www/app.css")),

  # when shiny is busy the following panels will create an overlay which will hide the main content until shiny is done computing
  # div(id = "busy"),
  # div(id = "BusyDIV"),

  # add help
  addIntroJS(useVoice=FALSE),

  # add notie type messages
  addNotieToPage()
)

load("dashboard_data.RData")



###CONNECT DATABASE
con <- dbConnect(RSQLite::SQLite(), "../app/data/SIMPOL.sqlite")

#####lOAD SIMPOL DASHBOAD DEFAULT DATA
load('./data/SIM_Dashboard.RData')


gaugedata <- readRDS("./data/gaugedata.RDS")
capacityTS <- readRDS("./data/TW/capacityTS.RDS")

###TEM LOAD PIE DATA
spillfreq <- unique(regatta_[c('joinSSC','SpillFreq')]) ##Get Unique SSC Spill frequency

##Plan , take top 4 -- rename as SSC , rest as Others
spills<-spillfreq[order(-spillfreq$SpillFreq),]
spills['SSC'] <-ifelse(spills$SpillFreq>=sort(spills$SpillFreq, TRUE)[4], spills$joinSSC, 'Others')
spills$SpillFreq <-as.double(spills$SpillFreq) ##convert to double from Int

pieData <-spills[,c('SSC','SpillFreq')]%>%
group_by(SSC)%>%
summarise(spills=sum(SpillFreq))%>%
pivot_wider(names_from = SSC, values_from = spills)

###SIMPOL RESULTS PROCESSING FUNCTION
calProcess <- function(simdata){

    ##remove empty columns
    simdata <- simdata[,names(simdata)!=""]

    simdata$`Date/Time`<- as.POSIXct(simdata$`Date/Time`,format="%d/%b/%Y %H:%M",tz='GMT')
    simLong  <- simdata%>%
    pivot_longer(cols= -`Date/Time`,names_to='Variable',values_to='value')
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    simLong$ModName <- trim(str_split_fixed(simLong$Variable," ",2)[,1]) #replaces old ifelse trip function
    simLong$VarName <- trim(str_split_fixed(simLong$Variable," ",2)[,2])

    ##select only useful columns
    sim <- simLong%>%select(`Date/Time`,Variable,ModName,VarName,value)
      ##convert time column then return final
    #sim$`Date/Time` <- as.POSIXct(sim$`Date/Time`,format="%d/%m/%Y %H:%M",tz='GMT')

    return (sim)

}

###TEMP
SSCResults <-calProcess(read.csv("C:\\Users\\ThinkPad\\Desktop\\alpha\\Phantom\\Rattlesnake\\Data\\Baseline 2016.csv",check.names=FALSE,skip=1))

# init
dataFilters   <-  list()
mapping       <-  list()

# init help
source("initHelp.R")
