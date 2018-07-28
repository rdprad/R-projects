library(ggplot2)
library(dplyr)
library(shiny)
library(shinycssloaders)
library(knitr)
library(shinythemes)
library(here)
library(ggiraph)
library(plotly)
library(kableExtra)

#test aja
BM_df  <- readRDS("~/ShinyApps/model_reconciliation/objects/BM_df.rds")
# BM_df  <- readRDS(here::here("objects", "BM_df.rds"))

ui <- fluidPage( theme = shinytheme("lumen"),
                 titlePanel("Block Model Spatial Reconciliation"),
                 sidebarLayout(
                   sidebarPanel(
                     selectizeInput('phaseselect', label = h4('Phase Selection (you may select more than one)'), choices = levels(factor(unique(BM_df$PhaseID))), multiple = TRUE, selected=c("Phase 2","Phase 4A")),
                     sliderInput("elevationselect", label = h4("Elevation"), min(BM_df$elevation), 
                                 max(BM_df$elevation), value = 930, step=15),
                     radioButtons("gradeitemselect", label = h4("Grade Item Selection"),
                                  choices = list("Copper" = "CU", "Gold" = "AU"),
                                  selected = NULL),
                     tags$div(h6(tags$i("Dashboard under development, report any issues to:",tags$code("Rama Pradnyana")))),
                     width=3
                   ),
                   mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Side by Side Comparison", fluidPage(
                                   fluidRow(
                                     column(width = 6, h5("Resource Model"),
                                            ggiraphOutput("Resource_plot", height = 400) %>% withSpinner(type = 8)),
                                     column(width = 6, h5("Ore Control Model"),
                                            ggiraphOutput("OC_plot", height = 400) %>% withSpinner(type = 8)),
                                     column(width = 6, h5("BM diff. (Ore Control [minus] Resource Model)"),
                                            ggiraphOutput("diff_plot", height = 400) %>% withSpinner(type = 8))
                                   ))),
                                 tabPanel("Grade Tonnage Curve",  fluidPage(
                                   fluidRow(
                                     column(width = 6, h5("Grade Tonnage Curve (NSR cut-off)"),
                                            plotlyOutput("GTcurve_plot1", height = 400) %>% withSpinner()),
                                     column(width = 6, h5("Cumulative Contained Metal (NSR cut-off)"),
                                            plotlyOutput("GTcurve_metal1", height = 400) %>% withSpinner())
                                   ),
                                   fluidRow(
                                     column(width = 6, h5("Grade Tonnage Curve (grade selection cut-off)"),
                                            plotlyOutput("GTcurve_plot2", height = 400) %>% withSpinner()),
                                     column(width = 6, h5("Cumulative Contained Metal (grade selection cut-off)"),
                                            plotlyOutput("GTcurve_metal2", height = 400) %>% withSpinner())
                                   ))),
                                 tabPanel("Grade Tonnage Table",  fluidPage(
                                   fluidRow(
                                     downloadButton("downloadData1", "Download Table Below as csv (NSR cut-off)"),
                                     column(width = 12,
                                            tableOutput("table1") %>% withSpinner()),
                                     br()
                                   ),
                                   fluidRow(
                                     downloadButton("downloadData2", "Download Table Below as csv (grade selection cut-off)"),
                                     column(width = 12,
                                            tableOutput("table2") %>% withSpinner()),
                                     br()
                                   )))
                     ), width=8
                   )
                 )
)

server <- function(input, output) {
  # Subset data
  selected_blocks <- reactive({
    req(input$phaseselect, input$elevationselect, input$gradeitemselect)
    BM_df %>%
      filter(PhaseID %in% input$phaseselect,
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
             RSCgradebin = as.numeric(as.character(cut(RSCgrade,c(-2,seq(0.1,1,by = 0.1),99), labels = seq(0,1,by = 0.1)))),
             OCgradebin  = as.numeric(as.character(cut(OCgrade,c(-2,seq(0.1,1,by = 0.1),99), labels = seq(0,1,by = 0.1)))),
             RSCNSRbin = as.numeric(as.character(cut(NSR,c(-999,5,6.35,seq(10,50,by = 5),9999), labels = c(0,5,6.35,seq(10,50,by = 5))))),
             OCNSRbin  = as.numeric(as.character(cut(BNSR,c(-999,5,6.35,seq(10,50,by = 5),9999), labels = c(0,5,6.35,seq(10,50,by = 5))))),
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
             ttip1 = paste0(PhaseID,"<br/>", "grade = ",round(RSCgrade,2)),
             ttip2 = paste0(PhaseID,"<br/>", "grade = ",round(OCgrade,2)),
             ttip3 = paste0(PhaseID,"<br/>", "grade diff = ",round(diff,2))
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
  
  GT_table1 <- reactive({
    req(input$phaseselect, input$elevationselect, input$gradeitemselect)
    merge(
      selected_blocks() %>%
        group_by(elevation,RSCNSRbin) %>% 
        mutate(massRSC = 10*10*15*BSG,
               RSC_select    = ifelse(input$gradeitemselect=="CU","CU","AU"),
               gradeitemRSC = ifelse(RSC_select=="CU",CU,AU)) %>% 
        summarise(RSC_tonnes = sum(massRSC),
                  RSC_metal = sum(gradeitemRSC*massRSC)) %>% 
        arrange(desc(RSCNSRbin)) %>% 
        mutate(RSC_cumtonnes = cumsum(RSC_tonnes),
               RSC_ave_grade = cumsum(RSC_metal)/RSC_cumtonnes,
               RSC_select    = ifelse(input$gradeitemselect=="CU","CU","AU"),
               RSC_metal     = ifelse(RSC_select=="CU", round(RSC_cumtonnes*RSC_ave_grade/100000,1),
                                      round(RSC_cumtonnes*RSC_ave_grade/31.10348/1000,2))) %>% 
        arrange(RSCNSRbin) %>% 
        rename(grade_bin=RSCNSRbin),
      
      selected_blocks() %>%
        group_by(elevation,OCNSRbin) %>% 
        mutate(massOC = 10*10*15*BSG,
               OC_select   = ifelse(input$gradeitemselect=="CU","CU","AU"),
               gradeitemOC = ifelse(OC_select=="CU",BCU,BAU)) %>% 
        summarise(OC_tonnes = sum(massOC),
                  OC_metal = sum(gradeitemOC*massOC)) %>% 
        arrange(desc(OCNSRbin)) %>% 
        mutate(OC_cumtonnes = cumsum(OC_tonnes),
               OC_ave_grade = cumsum(OC_metal)/OC_cumtonnes,
               OC_select    = ifelse(input$gradeitemselect=="CU","CU","AU"),
               OC_metal     = ifelse(OC_select=="CU", round(OC_cumtonnes*OC_ave_grade/100000,1),
                                     round(OC_cumtonnes*OC_ave_grade/31.10348/1000,2))) %>% 
        arrange(OCNSRbin) %>% 
        rename(grade_bin=OCNSRbin),
      by=c("elevation","grade_bin"))
  })
  
  GT_table2 <- reactive({
    req(input$phaseselect, input$elevationselect, input$gradeitemselect)
    merge(
      selected_blocks() %>%
        group_by(elevation,RSCgradebin) %>% 
        mutate(massRSC = 10*10*15*BSG,
               RSC_select    = ifelse(input$gradeitemselect=="CU","CU","AU"),
               gradeitemRSC = ifelse(RSC_select=="CU",CU,AU)) %>% 
        summarise(RSC_tonnes = sum(massRSC),
                  RSC_metal = sum(gradeitemRSC*massRSC)) %>% 
        arrange(desc(RSCgradebin)) %>% 
        mutate(RSC_cumtonnes = cumsum(RSC_tonnes),
               RSC_ave_grade = cumsum(RSC_metal)/RSC_cumtonnes,
               RSC_select    = ifelse(input$gradeitemselect=="CU","CU","AU"),
               RSC_metal     = ifelse(RSC_select=="CU", round(RSC_cumtonnes*RSC_ave_grade/100000,1),
                                      round(RSC_cumtonnes*RSC_ave_grade/31.10348/1000,2))) %>% 
        arrange(RSCgradebin) %>% 
        rename(grade_bin=RSCgradebin),
      
      selected_blocks() %>%
        group_by(elevation,OCgradebin) %>% 
        mutate(massOC = 10*10*15*BSG,
               OC_select   = ifelse(input$gradeitemselect=="CU","CU","AU"),
               gradeitemOC = ifelse(OC_select=="CU",BCU,BAU)) %>% 
        summarise(OC_tonnes = sum(massOC),
                  OC_metal = sum(gradeitemOC*massOC)) %>% 
        arrange(desc(OCgradebin)) %>% 
        mutate(OC_cumtonnes = cumsum(OC_tonnes),
               OC_ave_grade = cumsum(OC_metal)/OC_cumtonnes,
               OC_select    = ifelse(input$gradeitemselect=="CU","CU","AU"),
               OC_metal     = ifelse(OC_select=="CU", round(OC_cumtonnes*OC_ave_grade/100000,1),
                                     round(OC_cumtonnes*OC_ave_grade/31.10348/1000,2))) %>% 
        arrange(OCgradebin) %>% 
        rename(grade_bin=OCgradebin),
      by=c("elevation","grade_bin"))
  })
  
  #---
  output$GTcurve_plot1 <- renderPlotly({
    plot_ly() %>% 
      add_lines(x = GT_table1()$grade_bin, y = GT_table1()$OC_cumtonnes, name = "OC tonnes", line = list(color = 'rgb(22, 96, 167)', width=1),
                text=paste0("cut-off grade = ", GT_table1()$grade_bin, "<br>", "OC tonnes = ", format(round(GT_table1()$OC_cumtonnes,0), big.mark=",")),
                hoverinfo="text") %>%
      add_lines(x = GT_table1()$grade_bin, y = GT_table1()$RSC_cumtonnes, name = "Resource tonnes", line = list(color = 'rgb(22, 96, 167)', dash = 'dot', width=1),
                text=paste0("cut-off grade = ", GT_table1()$grade_bin, "<br>", "Resource tonnes = ", format(round(GT_table1()$RSC_cumtonnes,0), big.mark=",")),
                hoverinfo="text") %>%
      add_lines(x = GT_table1()$grade_bin, y = GT_table1()$OC_ave_grade, name = "OC grades", yaxis = "y2", line = list(color = 'rgb(205, 12, 24)', width=1),
                text=paste0("cut-off grade = ", GT_table1()$grade_bin, "<br>", "OC ave grades = ", round(GT_table1()$OC_ave_grade,2)),
                hoverinfo="text") %>%
      add_lines(x = GT_table1()$grade_bin, y = GT_table1()$RSC_ave_grade, name = "Resource grades", yaxis = "y2", line = list(color = 'rgb(205, 12, 24)', dash = 'dot', width=1),
                text=paste0("cut-off grade = ", GT_table1()$grade_bin, "<br>", "Resource ave grades = ", round(GT_table1()$RSC_ave_grade,2)),
                hoverinfo="text") %>%
      # add_segments(x=15, xend = 15, y=0, yend = max(GT_table1()$OC_cumtonnes), color="orange", name="MG", hoverinfo = "none",showlegend = F) %>% 
      layout(legend = list(orientation = 'h')) %>% 
      config(displayModeBar = F) %>%
      layout(xaxis=list(fixedrange=TRUE,
                        autotick = FALSE,
                        dtick = 5,
                        ticks = "outside",
                        tick0 = 0,
                        hoverformat = ".2f",
                        rangemode = "tozero",
                        showspikes = TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE,
                        title = "tonnes",
                        rangemode = "tozero",
                        showspikes = TRUE,
                        hoverformat = ",.0f",
                        tickformat = ",.0f")) %>% 
      layout(yaxis2=list(showgrid=FALSE,
                         title = 'average grade',
                         fixedrange=TRUE, 
                         autotick = FALSE,
                         tick0 = 0,
                         dtick = 0.1,
                         tickformat = ".2f",
                         hoverformat = ".2f",
                         overlaying = "y",
                         side = "right",
                         position = 0.95,
                         rangemode = "tozero",
                         showspikes = TRUE))%>% 
      layout(font= list(size=10),
             shapes = list(
               list(type = "rect",
                    fillcolor = "yellow", line = list(color = "transparent"), opacity = 0.2,
                    x0 = 15, x1 = 20, xref = "x",
                    y0 = 0, y1 = max(GT_table1()$OC_cumtonnes)+500000, yref = "y"),
               list(type = "rect",
                    fillcolor = "red", line = list(color = "transparent"), opacity = 0.2,
                    x0 = 20, x1 = 25, xref = "x",
                    y0 = 0, y1 = max(GT_table1()$OC_cumtonnes)+500000, yref = "y"),
               list(type = "rect",
                    fillcolor = "orange", line = list(color = "transparent"), opacity = 0.2,
                    x0 = 25, x1 = 50, xref = "x",
                    y0 = 0, y1 = max(GT_table1()$OC_cumtonnes)+500000, yref = "y"),
               list(type = "rect",
                    fillcolor = "green", line = list(color = "transparent"), opacity = 0.2,
                    x0 = 6.35, x1 = 15, xref = "x",
                    y0 = 0, y1 = max(GT_table1()$OC_cumtonnes)+500000, yref = "y"),
               list(type = "rect",
                    fillcolor = "grey", line = list(color = "transparent"), opacity = 0.2,
                    x0 = 0, x1 = 6.35, xref = "x",
                    y0 = 0, y1 = max(GT_table1()$OC_cumtonnes)+500000, yref = "y")
             ),
             annotations = list(
               list(x=3 , y=250000, text="WST", xref="x", yref="y", showarrow = FALSE),
               list(x=11, y=250000, text="LG",  xref="x", yref="y", showarrow = FALSE),
               list(x=17, y=250000, text="MG",  xref="x", yref="y", showarrow = FALSE),
               list(x=22, y=250000, text="HG",  xref="x", yref="y", showarrow = FALSE),
               list(x=27, y=250000, text="CRS", xref="x", yref="y", showarrow = FALSE),
               list(x=17, y=-200000, text="NSR($/t) cut-off", xref="x", yref="y", showarrow = FALSE)))
  })
  
  
  output$GTcurve_plot2 <- renderPlotly({
    plot_ly() %>% 
      add_lines(x = GT_table2()$grade_bin, y = GT_table2()$OC_cumtonnes, name = "OC tonnes", line = list(color = 'rgb(22, 96, 167)', width=1),
                text=paste0("cut-off grade = ", GT_table2()$grade_bin, "<br>", "OC tonnes = ", format(round(GT_table2()$OC_cumtonnes,0), big.mark=",")),
                hoverinfo="text") %>%
      add_lines(x = GT_table2()$grade_bin, y = GT_table2()$RSC_cumtonnes, name = "Resource tonnes", line = list(color = 'rgb(22, 96, 167)', dash = 'dot', width=1),
                text=paste0("cut-off grade = ", GT_table2()$grade_bin, "<br>", "Resource tonnes = ", format(round(GT_table2()$RSC_cumtonnes,0), big.mark=",")),
                hoverinfo="text") %>%
      add_lines(x = GT_table2()$grade_bin, y = GT_table2()$OC_ave_grade, name = "OC grades", yaxis = "y2", line = list(color = 'rgb(205, 12, 24)', width=1),
                text=paste0("cut-off grade = ", GT_table2()$grade_bin, "<br>", "OC ave grades = ", round(GT_table2()$OC_ave_grade,2)),
                hoverinfo="text") %>%
      add_lines(x = GT_table2()$grade_bin, y = GT_table2()$RSC_ave_grade, name = "Resource grades", yaxis = "y2", line = list(color = 'rgb(205, 12, 24)', dash = 'dot', width=1),
                text=paste0("cut-off grade = ", GT_table2()$grade_bin, "<br>", "Resource ave grades = ", round(GT_table2()$RSC_ave_grade,2)),
                hoverinfo="text") %>%
      layout(legend = list(orientation = 'h')) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=TRUE, 
                        autotick = FALSE,
                        ticks = "outside",
                        tick0 = 0,
                        dtick = 0.1,
                        hoverformat = ".2f",
                        rangemode = "tozero",
                        showspikes = TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE,
                        title = "tonnes",
                        rangemode = "tozero",
                        showspikes = TRUE,
                        hoverformat = ",.0f",
                        tickformat = ",.0f")) %>% 
      layout(yaxis2=list(showgrid=TRUE,
                         title = 'average grade',
                         fixedrange=TRUE, 
                         autotick = FALSE,
                         tick0 = 0,
                         dtick = 0.1,
                         tickformat = ".2f",
                         hoverformat = ".2f",
                         overlaying = "y",
                         side = "right",
                         position = 0.95,
                         rangemode = "tozero",
                         showspikes = TRUE))%>% 
      layout(font= list(size=10))
  })
  #---
  
  #---
  output$GTcurve_metal1 <- renderPlotly({
    plot_ly() %>% 
      add_lines(x = GT_table1()$grade_bin, y = GT_table1()$OC_metal, name = "OC metal", line = list(color = 'rgb(22, 96, 167)', width=1),
                text=paste0("cut-off grade = ", GT_table1()$grade_bin, "<br>", "OC metal = ", round(GT_table1()$OC_metal,2)),
                hoverinfo="text") %>%
      add_lines(x = GT_table1()$grade_bin, y = GT_table1()$RSC_metal, name = "Resource metal", line = list(color = 'rgb(205, 12, 24)', dash = 'dot', width=1),
                text=paste0("cut-off grade = ", GT_table1()$grade_bin, "<br>", "Resource metal = ", round(GT_table1()$RSC_metal,2)),
                hoverinfo="text") %>%
      layout(legend = list(orientation = 'h')) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=TRUE, 
                        autotick = FALSE,
                        dtick = 5,
                        ticks = "outside",
                        tick0 = 0,
                        rangemode = "tozero",
                        showspikes = TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE,
                        rangemode = "tozero",
                        showspikes = TRUE,
                        tickformat = ",.2f"))%>% 
      layout(font= list(size=10),
             shapes = list(
               list(type = "rect",
                    fillcolor = "yellow", line = list(color = "transparent"), opacity = 0.2,
                    x0 = 15, x1 = 20, xref = "x",
                    y0 = 0, y1 = max(GT_table1()$OC_metal,GT_table1()$RSC_metal)+2, yref = "y"),
               list(type = "rect",
                    fillcolor = "red", line = list(color = "transparent"), opacity = 0.2,
                    x0 = 20, x1 = 25, xref = "x",
                    y0 = 0, y1 = max(GT_table1()$OC_metal,GT_table1()$RSC_metal)+2, yref = "y"),
               list(type = "rect",
                    fillcolor = "orange", line = list(color = "transparent"), opacity = 0.2,
                    x0 = 25, x1 = 50, xref = "x",
                    y0 = 0, y1 = max(GT_table1()$OC_metal,GT_table1()$RSC_metal)+2, yref = "y"),
               list(type = "rect",
                    fillcolor = "green", line = list(color = "transparent"), opacity = 0.2,
                    x0 = 6.35, x1 = 15, xref = "x",
                    y0 = 0, y1 = max(GT_table1()$OC_metal,GT_table1()$RSC_metal)+2, yref = "y"),
               list(type = "rect",
                    fillcolor = "grey", line = list(color = "transparent"), opacity = 0.2,
                    x0 = 0, x1 = 6.35, xref = "x",
                    y0 = 0, y1 = max(GT_table1()$OC_metal,GT_table1()$RSC_metal)+2, yref = "y")
             ),
             annotations = list(
               list(x=3 , y=2, text="WST", xref="x", yref="y", showarrow = FALSE),
               list(x=11, y=2, text="LG",  xref="x", yref="y", showarrow = FALSE),
               list(x=17, y=2, text="MG",  xref="x", yref="y", showarrow = FALSE),
               list(x=22, y=2, text="HG",  xref="x", yref="y", showarrow = FALSE),
               list(x=27, y=2, text="CRS", xref="x", yref="y", showarrow = FALSE),
               list(x=17, y=-1, text="NSR($/t) cut-off", xref="x", yref="y", showarrow = FALSE)))
  })
  
  
  output$GTcurve_metal2 <- renderPlotly({
    plot_ly() %>% 
      add_lines(x = GT_table2()$grade_bin, y = GT_table2()$OC_metal, name = "OC metal", line = list(color = 'rgb(22, 96, 167)', width=1),
                text=paste0("cut-off grade = ", GT_table2()$grade_bin, "<br>", "OC metal = ", round(GT_table2()$OC_metal,2)),
                hoverinfo="text") %>%
      add_lines(x = GT_table2()$grade_bin, y = GT_table2()$RSC_metal, name = "Resource metal", line = list(color = 'rgb(205, 12, 24)', dash = 'dot', width=1),
                text=paste0("cut-off grade = ", GT_table2()$grade_bin, "<br>", "Resource metal = ", round(GT_table2()$RSC_metal,2)),
                hoverinfo="text") %>%
      layout(legend = list(orientation = 'h')) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=TRUE, 
                        autotick = FALSE,
                        ticks = "outside",
                        tick0 = 0,
                        dtick = 0.1,
                        rangemode = "tozero",
                        showspikes = TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE,
                        rangemode = "tozero",
                        title = "Cu (kt); Au (koz)",
                        showspikes = TRUE,
                        tickformat = ",.2f"))%>% 
      layout(font= list(size=10))
  })
  #---
  
  output$table1 <- function(){
    GT_table1() %>% 
      mutate(RSC_tonnes = round(RSC_cumtonnes,0),
             OC_tonnes  = round(OC_cumtonnes,0)) %>% 
      select(grade_bin, RSC_tonnes, RSC_ave_grade, OC_tonnes, OC_ave_grade, RSC_metal, OC_metal) %>%
      arrange(grade_bin) %>% 
      kable("html",format.args = list(decimal.mark = ".", big.mark = ","), digits = 3)%>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "left")
  }
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("GT_table1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(GT_table1(), file, row.names = FALSE)
    }
  )
  
  output$table2 <- function(){
    GT_table2() %>% 
      mutate(RSC_tonnes = round(RSC_cumtonnes,0),
             OC_tonnes  = round(OC_cumtonnes,0)) %>% 
      select(grade_bin, RSC_tonnes, RSC_ave_grade, OC_tonnes, OC_ave_grade, RSC_metal, OC_metal) %>%
      arrange(grade_bin) %>% 
      kable("html",format.args = list(decimal.mark = ".", big.mark = ","), digits = 3)%>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "left")
  }
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("GT_table2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(GT_table2(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)

