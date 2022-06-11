library(shiny)

options(shiny.port = 8889)

options(autoreload.pattern = glob2rx("modules/mapModule.R"))

runApp()
