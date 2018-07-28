library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shinycssloaders)

ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Truck Speed Viewer"),
                sidebarLayout(
                 sidebarPanel(fileInput("filesSelect", "Choose CSV files from directory",
                                        multiple = TRUE,
                                        accept = '.csv'),
                              verbatimTextOutput("sourced")#,
                              #uiOutput("slider")
                 ),
                 mainPanel(fluidPage(
                  fluidRow(
                   column(width = 8, class = "well",
                          h4("Truck Speed GPS Plot"),
                          fluidRow(
                           column(width = 12, h5("Full View"),
                                  plotOutput("truck", height = 500,
                                             brush = brushOpts(
                                              id = "plot_brush",
                                              resetOnNew = TRUE)) %>% 
                                   withSpinner(type = 8)))),
                   column(width = 8, class = "well",
                          fluidRow(
                           column(width = 8, h4("Zoomed Selection"),
                                  plotOutput("truck_zoom", height = 300) %>% withSpinner()),
                           column(width = 4, h4("Boxplot"),
                                  plotOutput("boxplot", height = 300) %>% withSpinner())
                          )))
                 ))
                )
)

server <- function(input,output){
 options(shiny.maxRequestSize=100*1024^2) #limit maximum files selection to 100MB
 
 header_keep <- reactive({
  inFile <- input$filesSelect
  if (is.null(inFile)) {
   return(NULL)
  } else {
   inFile %>% 
    rowwise() %>% 
    do ({
     read.csv(.$datapath,nrows = 1, header=F,
              stringsAsFactors = FALSE)
    })
  }
 })
 
 df <- reactive({
   inFile <- input$filesSelect
   if (is.null(inFile)) {
    return(NULL)
   } else {
    inFile %>% 
     rowwise() %>% 
     do ({
      read.csv(.$datapath,skip=2, header=F,
               stringsAsFactors = FALSE, col.names = header_keep()[1,])
     })
     # select(position_x, position_y, speed, opreason, equipment_model, cycle_stage) %>%

   }
 })
 
 df2 <- reactive({
  if (is.null(df())) {
   return(NULL)
  } else {
   df() %>% 
    mutate(speed_kph = speed*3.6) %>% 
    filter(grepl("OPERATING", opreason)) %>% 
    filter(grepl("930e",equipment_model)) %>% 
    filter(grepl("Haul",cycle_stage))
  }
  
 })
 
 output$sourced <- renderPrint({
  if (is.null(df2())) {
   return (noquote("Note: Max total file size = 100 MB"))
  } else {
   cat("number of data: \n",format(nrow(df2()),big.mark = ","),"points \n","Filesize:", 
       round(sum(input$filesSelect$size*10^-6),1),"MB \n",
       "Filtered by: \n",
       "opreason =", unique(df2()$opreason),"\n",
       "equipment_model =", unique(df2()$equipment_model),"\n",
       "cycle_stage =", unique(df2()$cycle_stage))
  }
 })
 
 # output$slider <- renderUI({
 #  sliderInput("slider","Slider", min = min(df()$start_time),
 #              max   = max((df()$start_time)),
 #              value = c(min(df()$start_time),
 #                        max(df()$start_time)),
 #              step=NULL)
 # })
 
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
 
 output$truck <- renderPlot({
  if (is.null(df2())) {
   return(NULL)
  } else {
   ggplot(df2(), aes(position_x, position_y)) +
    geom_point(aes(col=speed_kph), pch=20)+
    scale_color_gradientn(colours = rainbow(5))+
    coord_equal()+
    theme_bw()}
 }) 
 
 output$truck_zoom <- renderPlot({
  if (is.null(df2())) {
   return(NULL)
  } else {
  ggplot(df2(), aes(position_x, position_y))+
    geom_point(aes(col=speed_kph), pch=19)+
    guides(col=FALSE)+
    scale_color_gradientn(colours = rainbow(5))+
    coord_fixed(ratio = 1, xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
    theme_bw()}
 })
 
 output$boxplot <- renderPlot({
  if (is.null(df2())) {
   return(NULL)
  } else {
  ggplot(brushedPoints(df2(), input$plot_brush),aes(x=cycle_stage,y=speed_kph, group=cycle_stage))+
   geom_boxplot(fill="grey")+
   scale_y_continuous(limits=c(0,60), breaks = seq(0,60,by=5))+
   stat_summary(fun.y=mean, geom="point", shape=3, size=3, color="red") +
   stat_summary(aes(label=paste(round(..y..,1),"kph")), fun.y=mean, geom="text", size=4, vjust = -0.5, color="black")+
   theme_minimal()}
 })
 
}

shinyApp(ui, server)
