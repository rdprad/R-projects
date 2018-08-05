library(shiny)
library(shinythemes)
library(shinycssloaders)
library(maptools)
library(rgdal)
library(dplyr)

ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Shapefiles uploader"),
                sidebarLayout(
                 sidebarPanel(fileInput("shpFile", label = "Choose Shape files from directory",
                                        multiple = TRUE,
                                        accept = "gml",
                                        placeholder = "select all three file extension: .dbf, .shp, .shx")),
                 mainPanel(fluidPage(
                  fluidRow(
                   column(width = 8, class = "well",
                          h4("Shapefile Plot"),
                          fluidRow(
                           column(width = 12, h5("Full View"),
                                  plotOutput("SHPplot", height = 400) %>% 
                                   withSpinner(type = 8)))),
                   column(width = 8, class = "well",
                          fluidRow(
                           
                          )))
                 ))
                )
  
)

server <- function(input, output) {
 options(shiny.maxRequestSize=100*1024^2) #limit maximum files selection to 100MB
 
 uploadShpfile <- reactive({
  if (!is.null(input$shpFile)){
   shpDF <- input$shpFile
   prevWD <- getwd()
   uploadDirectory <- dirname(shpDF$datapath[1])
   setwd(uploadDirectory)
   for (i in 1:nrow(shpDF)){
    file.rename(shpDF$datapath[i], shpDF$name[i])
   }
   shpName <- c(shpDF$name[grep(x=shpDF$name, pattern="*.shp")],
                shpDF$name[grep(x=shpDF$name, pattern="*.dxf")])
   shpPath <- paste(uploadDirectory, shpName, sep ="/")
   setwd(prevWD)
   shpFile <- readOGR(shpPath)
   return(shpFile)
  } else {
   return()
  }
 })
 
 output$SHPplot <- renderPlot({
  if (!is.null(uploadShpfile())){
   plot(uploadShpfile())
  }
 }) 
 
}

shinyApp(ui, server)
