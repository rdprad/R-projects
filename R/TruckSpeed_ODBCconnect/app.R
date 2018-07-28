library(odbc)
library(lubridate)
library(ggplot2)
library(dplyr)
library(shiny)
library(shinycssloaders)

con <- dbConnect(odbc::odbc(), "SQLD1")
con_DF <- dbGetQuery(con, "select * FROM OpenPit.dbo.TruckSpeed" )
con_DF$speed_kph <- con_DF$speed*3.6


ui <- fluidPage(
 fluidRow(
  column(width = 8, class = "well",
         h4("Truck Speed GPS Plot"),
         fluidRow(
          column(width = 12, h5("Full View"),
                 plotOutput("truck", height = 500,
                            brush = brushOpts(
                             id = "plot_brush",
                             resetOnNew = TRUE)) %>% withSpinner(type = 8)))),
  column(width = 8, class = "well",
         fluidRow(
          column(width = 3, h4("Date Range"),
                 dateRangeInput("dates", label = NULL)),
          column(width = 5, h4("Zoomed Selection"),
                 plotOutput("truck_zoom", height = 300) %>% withSpinner()),
          column(width = 4, h4("Boxplot"),
                 plotOutput("boxplot", height = 300) %>% withSpinner())
         )))
)

server <- function(input, output){
 ranges <- reactiveValues(x = NULL, y = NULL)
 
 con_DF2 <- reactive ({con_DF %>% 
  filter(start_time >= input$dates[1]+(6/24), start_time<=input$dates[2]+(30/24))})

 output$truck <- renderPlot({
  ggplot(con_DF2(), aes(position_x, position_y)) +
   geom_point(aes(col=speed_kph), pch=20)+
   scale_color_gradientn(colours = rainbow(5))+
   coord_equal()+
   theme_bw()
 })
 
 output$truck_zoom <- renderPlot({
  ggplot(con_DF2(), aes(position_x, position_y)) +
   geom_point(aes(col=speed_kph), pch=19)+
   scale_color_gradientn(colours = rainbow(5))+
   coord_fixed(ratio = 1, xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
   theme_bw()
 })
 
 output$boxplot <- renderPlot({
  ggplot(brushedPoints(con_DF2(), input$plot_brush),aes(x=cycle_stage,y=speed_kph, group=cycle_stage))+
   geom_boxplot(fill="grey")+
   scale_y_continuous(limits=c(0,60), breaks = seq(0,60,by=5))+
   stat_summary(fun.y=mean, geom="point", shape=3, size=3, color="red") +
   stat_summary(aes(label=paste(round(..y..,1),"kph")), fun.y=mean, geom="text", size=4, vjust = -0.5, color="black")+
   theme_minimal()
 })
 
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
 
}


shinyApp(ui, server)  
