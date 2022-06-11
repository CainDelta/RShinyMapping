library(shiny)

options(shiny.port = 8888)
options(autoreload.pattern = glob2rx("modules/mapModule.R"))
runApp()
