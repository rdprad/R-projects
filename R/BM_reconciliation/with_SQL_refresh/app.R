library(ggplot2)
library(dplyr)
library(shiny)
library(shinycssloaders)
library(knitr)
library(shinythemes)
library(here)
library(ggiraph)
library(shinyWidgets)
library(plotly)

BM_df  <- readRDS(here::here("objects", "BM_df.rds"))

ui <- fluidPage( theme = shinytheme("lumen"),
                 titlePanel("Block Model Spatial Reconciliation"),
                 sidebarLayout(
                  sidebarPanel(
                   actionButton("refreshSQL", "Press to refresh SQL con, and wait until bar below shows 100%"),
                   progressBar(id = "pb2", value = 0, total = 100, title = "", display_pct = TRUE),
                   selectInput(inputId = "phaseselect", label = strong("Phase"),
                               choices = levels(factor(unique(BM_df$PhaseID))),
                               selected = "Phase 4A"),
                   sliderInput("elevationselect", label = h3("Elevation"), min(BM_df$elevation), 
                               max(BM_df$elevation), value = 960, step=15),
                   radioButtons("gradeitemselect", label = h3("Grade Item Selection"),
                                 choices = list("Copper" = "CU", "Gold" = "AU"),
                                 selected = NULL),
                   tableOutput("table"),
                   width=4
                  ),
                  mainPanel(
                   fluidPage(
                    fluidRow(
                     column(width = 12, class = "well",
                            h4("Side by Side Block Model Comparison"),
                            fluidRow(
                             column(width = 6, h5("Resource Model"),
                                    ggiraphOutput("Resource_plot", height = 450) %>% withSpinner(type = 8)),
                             column(width = 6, h5("Ore Control Model"),
                                    ggiraphOutput("OC_plot", height = 450) %>% withSpinner(type = 8)),
                             column(width = 6, h5("BM diff. (OC - RSC)"),
                                    ggiraphOutput("diff_plot", height = 450) %>% withSpinner(type = 8))
                             )),
                     column(width = 12, class = "well",
                            fluidRow(
                             column(width = 6, h5("Grade Tonnage Curve"),
                                    plotlyOutput("GTcurve_plot", height = 450) %>% withSpinner(type = 8)),
                             column(width = 6, h5("Grade Tonnage Curve"),
                                    plotlyOutput("GTcurve_metal", height = 450) %>% withSpinner(type = 8))
                            )))
                   )
                  )
                 )
                 
)

server <- function(input, output, session) {
 observeEvent(input$refreshSQL,{
  source(here::here("refreshSQL.R"))
  
  for (i in 1:100) {
   updateProgressBar(
    session = session,
    id = "pb2",
    value = i, total = 100,
    title = paste("Process", trunc(i/10))
   )
   Sys.sleep(0.1)
  }
  
  BM_df  <- readRDS(here::here("objects", "BM_df.rds"))
  })
 
 # Subset data
 selected_blocks <- reactive({
  req(input$phaseselect, input$elevationselect, input$gradeitemselect)
  BM_df %>%
   filter(PhaseID == input$phaseselect,
          elevation == input$elevationselect) %>% 
   mutate(grade_sel = input$gradeitemselect,
          diff = ifelse(grade_sel == "CU",BCU-CU,
                        ifelse(grade_sel=="AU",BAU-AU,
                               ifelse(grade_sel=="AG",BAG-AG,
                                      ifelse(grade_sel=="AS",BAS-AS,NA)))),
          RSCgrade = ifelse(grade_sel=="CU",CU,
                            ifelse(grade_sel=="AU",AU,
                                   ifelse(grade_sel=="AG",AG,
                                          ifelse(grade_sel=="AS",AS,NA)))),
          OCgrade = ifelse(grade_sel=="CU",BCU,
                            ifelse(grade_sel=="AU",BAU,
                                   ifelse(grade_sel=="AG",BAG,
                                          ifelse(grade_sel=="AS",BAS,NA)))),
          RSCbin =  ifelse(RSCgrade>=1.0,1.0,
                     ifelse(RSCgrade>=0.9,0.9,
                      ifelse(RSCgrade>=0.8,0.8,
                       ifelse(RSCgrade>=0.7,0.7,
                        ifelse(RSCgrade>=0.6,0.6,
                         ifelse(RSCgrade>=0.5,0.5,
                          ifelse(RSCgrade>=0.4,0.4,
                           ifelse(RSCgrade>=0.3,0.3,
                            ifelse(RSCgrade>=0.2,0.2,
                             ifelse(RSCgrade>=0.1,0.1,0)))))))))),
          OCbin  = ifelse(OCgrade>=1.0,1.0,
                    ifelse(OCgrade>=0.9,0.9,
                     ifelse(OCgrade>=0.8,0.8,
                      ifelse(OCgrade>=0.7,0.7,
                       ifelse(OCgrade>=0.6,0.6,
                        ifelse(OCgrade>=0.5,0.5,
                         ifelse(OCgrade>=0.4,0.4,
                          ifelse(OCgrade>=0.3,0.3,
                           ifelse(OCgrade>=0.2,0.2,
                            ifelse(OCgrade>=0.1,0.1,0)))))))))),
          RSClegendbin =  ifelse(RSCgrade>=1.0,"1.0 - max",
                           ifelse(RSCgrade>=0.8,"0.8 - 1.0",
                            ifelse(RSCgrade>=0.6,"0.6 - 0.8",
                             ifelse(RSCgrade>=0.4,"0.4 - 0.6",
                              ifelse(RSCgrade>=0.3,"0.3 - 0.4", 
                               ifelse(RSCgrade>=0.2,"0.2 - 0.3","0.0 - 0.2")))))),
          OClegendbin =   ifelse(OCgrade>=1.0,"1.0 - max",
                           ifelse(OCgrade>=0.8,"0.8 - 1.0",
                            ifelse(OCgrade>=0.6,"0.6 - 0.8",
                             ifelse(OCgrade>=0.4,"0.4 - 0.6",
                              ifelse(OCgrade>=0.3,"0.3 - 0.4", 
                               ifelse(OCgrade>=0.2,"0.2 - 0.3","0.0 - 0.2")))))),
          ttip1 = paste0("grade = ",round(RSCgrade,2)),
          ttip2 = paste0("grade = ",round(OCgrade,2)),
          ttip3 = paste0("grade diff = ",round(diff,2))
          )
 })
 
 #plot
 output$Resource_plot <- renderggiraph({
  a <-ggplot(selected_blocks(),aes(x=xcentre, y=ycentre))+
   geom_tile_interactive(aes(fill= RSClegendbin, tooltip = ttip1))+
   scale_fill_manual(values = c("grey", "green", "yellow", "orange", "red", "darkred","purple"))+
   coord_equal(ratio=1)+
   theme_bw()+
   labs(fill = "LEGEND")

  ggiraph(code = {print(a)}, tooltip_opacity = 0.5 , selection_type = "single")
 })
 
 output$OC_plot <- renderggiraph({
  b <- ggplot(selected_blocks(),aes(x=xcentre, y=ycentre))+
   geom_tile_interactive(aes(fill= OClegendbin, tooltip = ttip2))+
   scale_fill_manual(values = c("grey", "green", "yellow", "orange", "red", "darkred","purple"))+
   coord_equal(ratio=1)+
   theme_bw()+
   labs(fill = "LEGEND")
  
  ggiraph(code = {print(b)}, tooltip_opacity = 0.5 , selection_type = "single")
 })
 
 output$diff_plot <- renderggiraph({
  c <- ggplot(selected_blocks(),aes(x=xcentre, y=ycentre))+
   geom_tile_interactive(aes(fill= diff, tooltip = ttip3))+
   scale_fill_gradientn(colours = c("darkred", "red", "grey", "green", "darkgreen"),
                        values = scales::rescale(c(-1.5, -0.5, -0.2, 0, 0.2, 0.5, 1.5)),
                        guide = "colorbar", limits=c(-1.5,1.5))+
   coord_equal(ratio=1)+
   theme_bw()+
   labs(fill = "DIFFERENCE")
  
  ggiraph(code = {print(c)}, tooltip_opacity = 0.5 , selection_type = "single")
 })
 
 GT_table <- reactive({
  req(input$phaseselect, input$elevationselect, input$gradeitemselect)
  merge(
   selected_blocks() %>%
    group_by(elevation,RSCbin) %>% 
    mutate(massRSC = 10*10*15*BSG, 
           gradeitemRSC = ifelse(input$gradeitemselect=="CU",CU,AU)) %>% 
    summarise(RSC_tonnes = sum(massRSC),
              RSC_metal = sum(gradeitemRSC*massRSC)) %>% 
    arrange(desc(RSCbin)) %>% 
    mutate(RSC_cumtonnes = cumsum(RSC_tonnes),
           RSC_ave_grade = cumsum(RSC_metal)/RSC_cumtonnes,
           RSC_select    = ifelse(input$gradeitemselect=="CU","CU","AU"),
           RSC_metal     = ifelse(RSC_select=="CU", round(RSC_cumtonnes*RSC_ave_grade/100000,1),
                                  round(RSC_cumtonnes*RSC_ave_grade/31.10348/1000,1))) %>% 
    arrange(RSCbin) %>% 
    rename(grade_bin=RSCbin),
   
   selected_blocks() %>%
    group_by(elevation,OCbin) %>% 
    mutate(massOC = 10*10*15*BSG, 
           gradeitemOC = ifelse(input$gradeitemselect=="CU",BCU,BAU)) %>% 
    summarise(OC_tonnes = sum(massOC),
              OC_metal = sum(gradeitemOC*massOC)) %>% 
    arrange(desc(OCbin)) %>% 
    mutate(OC_cumtonnes = cumsum(OC_tonnes),
           OC_ave_grade = cumsum(OC_metal)/OC_cumtonnes,
           OC_select    = ifelse(input$gradeitemselect=="CU","CU","AU"),
           OC_metal     = ifelse(OC_select=="CU", round(OC_cumtonnes*OC_ave_grade/100000,1),
                                 round(OC_cumtonnes*OC_ave_grade/31.10348/1000,1))) %>% 
    arrange(OCbin) %>% 
    rename(grade_bin=OCbin),
   by=c("elevation","grade_bin"))
 })
 
 
 #---
 #  output$GTcurve_plot <- renderPlot({
 #  ggplot(data= GT_table(), aes(x=grade_bin))+
 #   geom_line(aes(y=OC_cumtonnes, col="OC tonnes (solid)"), linetype = "solid")+
 #   geom_line(aes(y=RSC_cumtonnes, col="Resource tonnes (dash)"), linetype = "dashed")+
 #   geom_line(aes(y=OC_ave_grade*max(GT_table()$RSC_cumtonnes), col="OC grades (solid)"), linetype = "solid")+
 #   geom_line(aes(y=RSC_ave_grade*max(GT_table()$RSC_cumtonnes), col="Resource grades (dash)"), linetype = "dashed")+
 #   scale_color_manual(values=c("red","blue","red","blue"))+
 #   scale_x_continuous(breaks = seq(min(GT_table()$grade_bin), max(GT_table()$grade_bin), by=0.1))+
 #   scale_y_continuous(labels = scales::comma, breaks = seq(0,max(GT_table()$RSC_cumtonnes),by=500000), 
 #                      sec.axis = sec_axis(~./(max(GT_table()$RSC_cumtonnes)), name = "Average Grade",
 #                                          breaks = seq(0,round(max(GT_table()$RSC_ave_grade),1),by=0.1)))+
 #   labs(x="cut-off head grade",
 #        y="Tonnage",
 #        col="LEGENDS")+
 #   theme_bw()+
 #   theme(legend.position = "bottom")
 # })
 #---
 
 #---
 output$GTcurve_plot <- renderPlotly({
  plot_ly() %>% 
   add_lines(x = GT_table()$grade_bin, y = GT_table()$OC_cumtonnes, name = "OC tonnes", line = list(color = 'rgb(22, 96, 167)')) %>%
   add_lines(x = GT_table()$grade_bin, y = GT_table()$RSC_cumtonnes, name = "Resource tonnes", line = list(color = 'rgb(22, 96, 167)', dash = 'dot')) %>%
   add_lines(x = GT_table()$grade_bin, y = GT_table()$OC_ave_grade, name = "OC grades", yaxis = "y2", line = list(color = 'rgb(205, 12, 24)')) %>%
   add_lines(x = GT_table()$grade_bin, y = GT_table()$RSC_ave_grade, name = "Resource grades", yaxis = "y2", line = list(color = 'rgb(205, 12, 24)', dash = 'dot')) %>%
   layout(legend = list(orientation = 'h')) %>% 
   config(displayModeBar = F) %>% 
   layout(xaxis=list(fixedrange=TRUE, 
                     autotick = FALSE,
                     ticks = "outside",
                     tick0 = 0,
                     dtick = 0.1,
                     showspikes = TRUE)) %>% 
   layout(yaxis=list(fixedrange=TRUE,
                     showspikes = TRUE,
                     tickformat = ",.0f")) %>% 
   layout(yaxis2=list(fixedrange=TRUE, 
                      autotick = FALSE,
                      tick0 = 0,
                      dtick = 0.1,
                      tickformat = ".2f",
                      overlaying = "y",
                      side = "right",
                      showspikes = TRUE))
 })
 #---
 
 #---
 output$GTcurve_metal <- renderPlotly({
  plot_ly() %>% 
   add_lines(x = GT_table()$grade_bin, y = GT_table()$OC_metal, name = "OC metal", line = list(color = 'rgb(22, 96, 167)')) %>%
   add_lines(x = GT_table()$grade_bin, y = GT_table()$RSC_metal, name = "Resource metal", line = list(color = 'rgb(205, 12, 24)', dash = 'dot')) %>%
   layout(legend = list(orientation = 'h')) %>% 
   config(displayModeBar = F) %>% 
   layout(xaxis=list(fixedrange=TRUE, 
                     autotick = FALSE,
                     ticks = "outside",
                     tick0 = 0,
                     dtick = 0.1,
                     showspikes = TRUE)) %>% 
   layout(yaxis=list(fixedrange=TRUE,
                     showspikes = TRUE,
                     tickformat = ",.2f"))
 })
#---
 
 output$table <- function(){
  GT_table() %>% 
   mutate(RSC_tonnes = round(RSC_cumtonnes,0),
          OC_tonnes  = round(OC_cumtonnes,0)) %>% 
   select(grade_bin, RSC_tonnes, RSC_ave_grade, OC_tonnes, OC_ave_grade, RSC_metal, OC_metal) %>% 
   kable("html",format.args = list(decimal.mark = ".", big.mark = ","), digits = 3)
  }
 
}

shinyApp(ui, server)

