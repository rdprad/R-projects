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
                            column(width = 7, class = "well",
                                   h4("Shapefile Plot"),
                                   fluidRow(
                                       column(width = 12, h5("Full View"),
                                              plotOutput("SHPplot", height = 400,
                                                         brush = brushOpts(
                                                             id="plot_brush",
                                                             resetOnNew = TRUE)) %>% 
                                                  withSpinner(type=8))
                                   )),
                            column(width = 7, class = "well",
                                   fluidRow(
                                       column(width = 12, h5("zoomed in view"),
                                              plotOutput("SHPplot_zoom", height=400) %>% 
                                                  withSpinner(type=8))
                                       
                                   )))
                    ))
                )#,
                #   tags$footer("My footer", align = "center", style = "
                # position:absolute;
                # bottom:0;
                # width:100%;
                # height:50px;   /* Height of the footer */
                # color: black;
                # padding: 10px;
                # background-color: white;
                # z-index: 1000;")
                
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
            ggplot(uploadShpfile())+
                geom_path(aes(x = long, y = lat, group = group),
                          color = 'purple', size = .5, alpha=0.5)+
                coord_equal(ratio = 1)+
                theme_minimal()
        }
    }) 
    
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    observe({
        brush <- input$plot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    
    output$SHPplot_zoom <- renderPlot({
        if (!is.null(uploadShpfile())){
            ggplot(uploadShpfile())+
                geom_path(aes(x = long, y = lat, group = group),
                          color = 'purple', size = .5, alpha=0.7)+
                coord_equal(ratio = 1, xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
                theme_minimal()}
        
    })
    
}

shinyApp(ui, server)
