shinyUI(

  navbarPage(id="main", windowTitle = "SIMPOL Visualisation", position = "fixed-top", title = NULL, header = FrissHeader,

     # tabPanel(title = "dashboard", value="Dashboard", icon = icon("dashboard"),
     #          addMessageBoxToPage(),
     #          frontPanelModuleUI("Dashboard")
     # ),

     ### SIMPOL Dashboard
     tabPanel(title = "SIMPOL", value="SIMPOL", icon = icon("dashboard"),
              addMessageBoxToPage(),
              loadEChartsLibrary(),
              frontSIMPOLModuleUI("SIMPOL")
     ),



     ###Maps Navbar
     navbarMenu("Spatial",icon=icon("map"),
     tabPanel(title = "Map",value="MapModule", icon = icon("map"),mapModuleUI("MapModule")),
     tabPanel(title = "Spills",value="SpillMapModule", icon = icon("map"),spillMapModuleUI("SpillMapModule")),
     tabPanel(title = "Quality",value="QualityMapModule", icon = icon("map"),qualityMapModuleUI("QualityMapModule",stats))
   ),
   tabPanel(title='Analysis',icon=icon("chart-bar"),value="Analysis",analysisModuleUI("Analysis",stats)),

   tabPanel(title = "Forecasting", value="Forecasting", icon = icon("chart-line"),
   forecastModuleUI("ForecastModule")
   ),

    ####FILE UPLOAD MENU
    tabPanel(title= "DataUpload",icon=icon("cloud-upload-alt"),value="UploadDataModule",
        uploadDataModuleUI("UploadDataModule")

      ),
    tabPanel(title='Forecast Data',icon=icon("cloud-download-alt"),value="DownloadData",dlForecastUI("DownloadData"))
  )
)
