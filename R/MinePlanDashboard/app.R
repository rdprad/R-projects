library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(ggiraph)
library(shinycssloaders)
library(plotly)
library(rCharts)
library(dplyr)
library(here)
library(ggiraph)

a <- readRDS(here::here("objects", "a.rds"))
b <- readRDS(here::here("objects", "b.rds"))
d <- readRDS(here::here("objects", "d.rds"))
d1 <- readRDS(here::here("objects", "d1.rds"))
d2 <- readRDS(here::here("objects", "d2.rds"))
phase_tbl <- readRDS(here::here("objects", "phase_tbl.rds"))


ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Mine Plan Dashboard"),
                mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel("Year 2019",  fluidPage(
                                fluidRow(
                                  column(width = 12),
                                  fluidRow(
                                    column(width = 8,
                                           fluidRow(
                                             column(width = 12,
                                                    column(valueBoxOutput("tmm"), width = 2),
                                                    column(valueBoxOutput("con"), width = 2),
                                                    column(valueBoxOutput("crusher"), width = 2),
                                                    column(valueBoxOutput("copper"), width = 2),
                                                    column(valueBoxOutput("gold"), width = 2),
                                                    column(valueBoxOutput("silver"), width = 2)),
                                             column(width = 12, HTML('<hr style="color: black;">')),
                                             column(width = 6, h4("Total Material Movement (TMM)"),
                                                    showOutput("chart_2019_tmm", "highcharts") %>% withSpinner(),
                                                    plotOutput("plot5", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>')),
                                             column(width = 6, h4("Mill Feed by Phase"),
                                                    showOutput("chart_2019_crs", "highcharts")%>% withSpinner(),
                                                    plotOutput("plot6", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>')),
                                             column(width = 6, h4("Copper/Gold Feed Grade"),
                                                    showOutput("chart_2019_line_1", 'highcharts')%>% withSpinner(),
                                                    plotOutput("plot7", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>')),
                                             column(width = 6, h4("Copper/Gold Metal Production"),
                                                    showOutput("chart_2019_line_2", 'highcharts')%>% withSpinner(),
                                                    plotOutput("plot8", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>'))
                                           )),
                                    column(width = 4, 
                                           fluidRow(
                                             column(width = 12, 
                                                    includeMarkdown(here::here("note1.md"))),
                                             column(width = 12,
                                                    showOutput("chart_2019_pie_1", 'polycharts'),
                                                    HTML('<style>.rChart {width: 100%}</style>')),
                                             column(width = 12, 
                                                    includeMarkdown(here::here("note2.md")))
                                           ))
                                  )))),
                              tabPanel("Custom Period Selection",  fluidPage(
                                fluidRow(
                                  column(width = 12),
                                  fluidRow(
                                    column(width = 8,
                                           fluidRow(
                                             column(width = 12,
                                                    column(valueBoxOutput("tmm2"), width = 2),
                                                    column(valueBoxOutput("con2"), width = 2),
                                                    column(valueBoxOutput("crusher2"), width = 2),
                                                    column(valueBoxOutput("copper2"), width = 2),
                                                    column(valueBoxOutput("gold2"), width = 2),
                                                    column(valueBoxOutput("silver2"), width = 2)),
                                             column(width = 12, HTML('<hr style="color: black;">')),
                                             column(width = 6, h4("Total Material Movement (TMM)"),
                                                    showOutput("chart_custom_tmm", "highcharts")%>% withSpinner(),
                                                    plotOutput("plot11", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>')),
                                             column(width = 6, h4("Mill Feed by Phase"),
                                                    showOutput("chart_custom_crs", "highcharts")%>% withSpinner(),
                                                    plotOutput("plot12", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>')),
                                             column(width = 6, h4("Copper/Gold Feed Grade"),
                                                    showOutput("chart_custom_line_1", 'highcharts')%>% withSpinner(),
                                                    plotOutput("plot13", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>')),
                                             column(width = 6, h4("Copper/Gold Metal Production"),
                                                    showOutput("chart_custom_line_2", 'highcharts')%>% withSpinner(),
                                                    plotOutput("plot14", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>'))
                                           )),
                                    column(width = 4,
                                           fluidRow(
                                             column(width = 12, 
                                                    includeMarkdown(here::here("note_custom.md"))),
                                             column(width = 12,sliderTextInput(
                                               inputId = "mySliderText",
                                               label = "",
                                               choices = levels(a$PeriodID),
                                               selected = levels(a$PeriodID)[c(4, 15)])),
                                             column(width = 12, HTML('<hr style="color: black;">')),
                                             column(width = 12, h4("Phase Development"),
                                                    ggiraphOutput("phase", height = 400)%>% withSpinner()),
                                             column(width = 12, h4("Data Table"),
                                                    DT::dataTableOutput("phasetable"))
                                           ))
                                  )))),
                              tabPanel("5 Years Horizon", fluidPage(
                                fluidRow(
                                  column(width = 12),
                                  fluidRow(
                                    column(width = 8,
                                           fluidRow(
                                             column(width = 6, h4("Total Material Movement (TMM)"),
                                                    showOutput("chart_5years_tmm", "highcharts")%>% withSpinner(),
                                                    plotOutput("plot1", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>')),
                                             column(width = 6, h4("Mill/Crusher Throughput"),
                                                    showOutput("chart_5years", "highcharts")%>% withSpinner(),
                                                    plotOutput("plot2", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>')),
                                             column(width = 6, h4("Copper/Gold Feed Grade"),
                                                    showOutput("chart_5years_line_1", 'highcharts')%>% withSpinner(),
                                                    plotOutput("plot3", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>')),
                                             column(width = 6, h4("Copper/Gold Metal Production"),
                                                    showOutput("chart_5years_line_2", 'highcharts')%>% withSpinner(),
                                                    plotOutput("plot4", height = "1px"),
                                                    HTML('<style>.rChart {width: 100%}</style>'))
                                           )),
                                    column(width = 4,
                                           fluidRow(
                                             column(width = 12, 
                                                    includeMarkdown(here::here("note_5years.md")))
                                           ))
                                  ))))
                  ), width = 12)
                
                
)

server <- function(input, output, session) {
  
  output$chart_2019_tmm <- renderChart2({
    # COUNTRY = input$country
    # country = subset(dat2m, country == COUNTRY)
    # p2 <- rPlot(value ~ year, color = 'gender', type = 'line', data = country)
    # p2$guides(y = list(min = 0, title = ""))
    # p2$guides(y = list(title = ""))
    # p2$addParams(height = 300, dom = 'chart2')
    
    p1 <- hPlot(TMM ~ PeriodID, data = subset(a, Year==2019), type = 'column', group = "Area", group.na = 'NA\'s')
    p1$plotOptions(column = list(stacking = "normal"))
    p1$colors('rgba(255, 0, 0, 0.5)', 'rgba(255, 127, 0, 0.5)', 'rgba(255, 255, 0, 0.5)', 'rgba(0, 255, 0, 0.5)',
              'rgba(0, 127, 255, 0.5)', 'rgba(0, 255, 255, 0.5)', 'rgba(127, 0, 255, 0.5)', 
              'rgba(127, 127, 127, 0.5)','rgba(0, 0, 0, 0.5)')
    # p1$legend(align = 'right', verticalAlign = 'top', layout = 'vertical')
    p1$set(width = session$clientData$output_plot5_width)
    return(p1)
  })
  
  
  output$chart_2019_crs <- renderChart2({
    p1 <- hPlot(Crusher_t ~ PeriodID, data = subset(a, Year==2019), type = 'column', group = "Area", group.na = 'NA\'s')
    p1$plotOptions(column = list(stacking = "normal"))
    p1$colors('rgba(255, 0, 0, 0.5)', 'rgba(255, 127, 0, 0.5)', 'rgba(255, 255, 0, 0.5)', 'rgba(0, 255, 0, 0.5)',
              'rgba(0, 127, 255, 0.5)', 'rgba(0, 255, 255, 0.5)', 'rgba(127, 0, 255, 0.5)', 
              'rgba(127, 127, 127, 0.5)','rgba(0, 0, 0, 0.5)')
    p1$set(width = session$clientData$output_plot6_width)
    return(p1)
  })
  
  output$chart_2019_line_1 <- renderChart2({
    p3 = hPlot(grade ~ PeriodID, data = rbind(subset(d1, Year==2019),subset(d2, Year==2019)), type = 'line', group = "group")
    p3$set(pointSize = 0, lineWidth = 1)
    p3$colors('rgba(0, 150, 188, 0.5)', 'rgba(255, 127, 0, 0.5)')
    p3$set(width = session$clientData$output_plot7_width)
    # p3$chart(showDistX = TRUE, showDistY = TRUE)
    return(p3)
  })
  
  output$chart_2019_line_2 <- renderChart2({
    p3 = hPlot(metal ~ PeriodID, data = rbind(subset(d1, Year==2019),subset(d2, Year==2019)), type = 'line', group = "group")
    p3$set(pointSize = 0, lineWidth = 1)
    p3$colors('rgba(0, 150, 188, 0.5)', 'rgba(255, 127, 0, 0.5)')
    p3$set(width = session$clientData$output_plot8_width)
    return(p3)
  })
  
  output$chart_2019_pie_1 <- renderChart2({
    x = subset(b, Year == 2019)
    p4 = hPlot(x = "Area", y = "Crusher_t", data = x, type = "pie")
    p4$colors('rgba(127, 127, 127, 0.5)','rgba(0, 0, 0, 0.5)', 'rgba(255, 0, 0, 0.5)', 
              'rgba(255, 127, 0, 0.5)', 'rgba(255, 255, 0, 0.5)', 'rgba(0, 255, 255, 0.5)')
    return(p4)
  })
  
  output$tmm <- renderValueBox({
    valueBox(
      "TMM",
      "128,017 ktonnes", icon = icon("pie-chart fa-2x")
    )
  })  
  
  output$crusher <- renderValueBox({
    valueBox(
      "Tput",
      "40,699 ktonnes", icon = icon("line-chart fa-2x")
    )
  })
  
  output$con <- renderValueBox({
    valueBox(
      "Con",
      "655 ktonnes", icon = icon("cart-plus fa-2x")
    )
  })
  
  output$copper <- renderValueBox({
    valueBox(
      "Copper",
      "147 ktonnes", icon = icon("dollar fa-2x")
    )
  })
  
  output$gold <- renderValueBox({
    valueBox(
      "Gold", 
      "195 koz", icon = icon("dollar fa-2x")
    )
  })
  
  output$silver <- renderValueBox({
    valueBox(
      "Silver",
      "1,095 koz", icon = icon("dollar fa-2x")
    )
  })
  
  
  # output for custom period selection---------------------------
  
  output$chart_custom_tmm <- renderChart2({
    x = a[c(min(grep(input$mySliderText[1],a$PeriodID)):max(grep(input$mySliderText[2],a$PeriodID))),]
    p1 <- hPlot(TMM ~ PeriodID, data = x, type = 'column', group = "Area", group.na = 'NA\'s')
    p1$plotOptions(column = list(stacking = "normal"))
    p1$colors('rgba(255, 0, 0, 0.5)', 'rgba(255, 127, 0, 0.5)', 'rgba(255, 255, 0, 0.5)', 'rgba(0, 255, 0, 0.5)',
              'rgba(0, 127, 255, 0.5)', 'rgba(0, 255, 255, 0.5)', 'rgba(127, 0, 255, 0.5)', 
              'rgba(127, 127, 127, 0.5)','rgba(0, 0, 0, 0.5)')
    # p1$legend(align = 'right', verticalAlign = 'top', layout = 'vertical')
    p1$set(width = session$clientData$output_plot11_width)
    p1$print(include_assets=T)
    return(p1)
  })
  
  output$chart_custom_crs <- renderChart2({
    x = a[c(min(grep(input$mySliderText[1],a$PeriodID)):max(grep(input$mySliderText[2],a$PeriodID))),]
    p1 <- hPlot(Crusher_t ~ PeriodID, data = x, type = 'column', group = "Area", group.na = 'NA\'s')
    p1$plotOptions(column = list(stacking = "normal"))
    p1$colors('rgba(255, 0, 0, 0.5)', 'rgba(255, 127, 0, 0.5)', 'rgba(255, 255, 0, 0.5)', 'rgba(0, 255, 0, 0.5)',
              'rgba(0, 127, 255, 0.5)', 'rgba(0, 255, 255, 0.5)', 'rgba(127, 0, 255, 0.5)', 
              'rgba(127, 127, 127, 0.5)','rgba(0, 0, 0, 0.5)')
    p1$set(width = session$clientData$output_plot12_width)
    return(p1)
  })
  
  output$chart_custom_line_1 <- renderChart2({
    x2 = rbind(d1[c(min(grep(input$mySliderText[1],d1$PeriodID)):max(grep(input$mySliderText[2],d1$PeriodID))),],
               d2[c(min(grep(input$mySliderText[1],d2$PeriodID)):max(grep(input$mySliderText[2],d2$PeriodID))),])
    
    p3 = hPlot(grade ~ PeriodID, data = x2, type = 'line', group = "group")
    p3$set(pointSize = 0, lineWidth = 1)
    p3$colors('rgba(0, 150, 188, 0.5)', 'rgba(255, 127, 0, 0.5)')
    p3$set(width = session$clientData$output_plot13_width)
    return(p3)
  })
  
  output$chart_custom_line_2 <- renderChart2({
    x2 = rbind(d1[c(min(grep(input$mySliderText[1],d1$PeriodID)):max(grep(input$mySliderText[2],d1$PeriodID))),],
               d2[c(min(grep(input$mySliderText[1],d2$PeriodID)):max(grep(input$mySliderText[2],d2$PeriodID))),])
    p3 = hPlot(metal ~ PeriodID, data = x2, type = 'line', group = "group")
    p3$set(pointSize = 0, lineWidth = 1)
    p3$colors('rgba(0, 150, 188, 0.5)', 'rgba(255, 127, 0, 0.5)')
    p3$set(width = session$clientData$output_plot14_width)
    return(p3)
  })
  
  table1 <- reactive({
    x3 = d[c(min(grep(input$mySliderText[1],d$PeriodID)):max(grep(input$mySliderText[2],d$PeriodID))),]
    x3 %>% 
      # mutate(TMM2 = ifelse(Area == "UG", 0, TMM)) %>% 
      summarise(TMM = round(sum(TMM),0),
                Crusher_t = round(sum(Crusher_t),0),
                Con_kt = round(sum(Con_kt),0),
                Cu_kt = round(sum(Cu_kt),0),
                Au_koz = round(sum(Au_koz),0),
                Ag_koz = round(sum(Ag_koz),0))
  })
  
  output$tmm2 <- renderValueBox({
    valueBox(
      "TMM",
      paste0(format(table1()$TMM, big.mark = ",")," ktonnes"), icon = icon("pie-chart fa-2x")
    )
  })  
  
  output$crusher2 <- renderValueBox({
    valueBox(
      "Tput",
      paste0(format(table1()$Crusher_t, big.mark = ",")," ktonnes"), icon = icon("line-chart fa-2x")
    )
  })
  
  output$con2 <- renderValueBox({
    valueBox(
      "Con",
      paste0(format(table1()$Con_kt, big.mark = ",")," ktonnes"), icon = icon("cart-plus fa-2x")
    )
  })
  
  output$copper2 <- renderValueBox({
    valueBox(
      "Copper",
      paste0(format(table1()$Cu_kt, big.mark = ",")," ktonnes"), icon = icon("dollar fa-2x")
    )
  })
  
  output$gold2 <- renderValueBox({
    valueBox(
      "Gold", 
      paste0(format(table1()$Au_koz, big.mark = ",")," koz"), icon = icon("dollar fa-2x")
    )
  })
  
  output$silver2 <- renderValueBox({
    valueBox(
      "Silver",
      paste0(format(table1()$Ag_koz, big.mark = ",")," koz"), icon = icon("dollar fa-2x")
    )
  })
  
  output$phase <- renderggiraph({
    jColors <- phase_tbl$color
    names(jColors) <- phase_tbl$Area
    
    x = phase_tbl[c(min(grep(input$mySliderText[1],phase_tbl$PeriodID)):max(grep(input$mySliderText[2],phase_tbl$PeriodID))),]
    mytheme <- theme_minimal() + theme(panel.grid = element_blank())
    
    gga <- x %>% 
      group_by(Area, Level) %>% 
      summarise(TMM = round(sum(TMM)/10^3,0)) %>% 
      filter(TMM >0) %>% 
      ggplot(aes(x=Area, y = Level))+
      geom_tile_interactive(aes(fill=Area, width=0.7, height=14, alpha=0.5,
                                tooltip = paste0(Area,"<br/>",format(round(TMM,0), big.mark = ","), " ktonnes", "<br/>", "Elevation = ", Level)))+
      scale_fill_manual(values = jColors)+
      scale_y_continuous(breaks=seq(720,1170,by=15))+
      mytheme+
      guides(fill=FALSE, alpha=F)+
      labs(x= "",
           y= "")
    
    ggiraph(code = {print(gga)}, tooltip_opacity = 0.5 , selection_type = "single")
  }) 
  
  # output for 5 Years Horizon tab---------------------------
  output$chart_5years <- renderChart2({
    p2 <- hPlot(Crusher_t ~ Year, data = subset(b, Year>2018), type = 'column', group = "Area", group.na = 'NA\'s')
    p2$plotOptions(column = list(stacking = "normal"))
    p2$colors('rgba(255, 0, 0, 0.5)', 'rgba(255, 127, 0, 0.5)', 'rgba(255, 255, 0, 0.5)', 'rgba(0, 255, 0, 0.5)',
              'rgba(0, 255, 255, 0.5)', 'rgba(127, 0, 255, 0.5)', 
              'rgba(127, 127, 127, 0.5)','rgba(0, 0, 0, 0.5)')
    p2$set(width = session$clientData$output_plot2_width)
    return(p2)})
  
  output$chart_5years_tmm <- renderChart2({
    p2 <- hPlot(TMM ~ Year, data = subset(b, Year>2018), type = 'column', group = "Area", group.na = 'NA\'s')
    p2$plotOptions(column = list(stacking = "normal"))
    p2$colors('rgba(255, 0, 0, 0.5)', 'rgba(255, 127, 0, 0.5)', 'rgba(255, 255, 0, 0.5)', 'rgba(0, 255, 0, 0.5)',
              'rgba(0, 255, 255, 0.5)', 'rgba(127, 0, 255, 0.5)', 
              'rgba(127, 127, 127, 0.5)','rgba(0, 0, 0, 0.5)')
    p2$set(width = session$clientData$output_plot1_width)
    return(p2)})
  
  table2 <- reactive({
    rbind(d %>% 
            group_by(Year) %>% 
            summarise(TMM = round(sum(TMM),0),
                      Crusher = round(sum(Crusher_t),0),
                      Con_kt = round(sum(Con_kt),0),
                      metal = round(sum(Cu_kt),0),
                      grade = round(weighted.mean(Cu_pct, Crusher_t),2),
                      group = "Copper"),
          d %>% 
            group_by(Year) %>% 
            summarise(TMM = round(sum(TMM),0),
                      Crusher = round(sum(Crusher_t),0),
                      Con_kt = round(sum(Con_kt),0),
                      metal = round(sum(Au_koz),0),
                      grade = round(weighted.mean(Au_ppm, Crusher_t),2),
                      group = "Gold")
    )
  })
  
  output$chart_5years_line_1 <- renderChart2({
    p3 = hPlot(grade ~ Year, data = subset(table2(), Year>2018), type = 'line', group = "group")
    p3$set(pointSize = 0, lineWidth = 1)
    p3$colors('rgba(0, 150, 188, 0.5)', 'rgba(255, 127, 0, 0.5)')
    p3$set(width = session$clientData$output_plot3_width)
    return(p3)
  })
  
  output$chart_5years_line_2 <- renderChart2({
    p3 = hPlot(metal ~ Year, data = subset(table2(), Year>2018), type = 'line', group = "group")
    p3$set(pointSize = 0, lineWidth = 1)
    p3$colors('rgba(0, 150, 188, 0.5)', 'rgba(255, 127, 0, 0.5)')
    p3$set(width = session$clientData$output_plot4_width)
    return(p3)
  })
  
  # Data Table-----------------
  tablephs <- reactive({
    x = a[c(min(grep(input$mySliderText[1],a$PeriodID)):max(grep(input$mySliderText[2],a$PeriodID))),]
    x$Area <- factor(x$Area, levels = c("Phs04a", "Phs04b", "Phs04b-n", "Phs05", "Phs06", "Phs06b", "Phs07", "Rehandle", "UG"))
    x %>% 
      group_by(PeriodID, Area) %>% 
      summarise(TMM = format(round(sum(TMM),0), big.mark = ","),
                mill = format(round(sum(Crusher_t),0), big.mark = ","),
                Cu = round(weighted.mean(Cu_pct, Crusher_t),3),
                Au = round(weighted.mean(Au_ppm, Crusher_t),3)) %>% 
      na.omit() %>% 
      # replace(.,is.na(.),0) %>% 
      arrange(Area)
  })
  
  output$phasetable <- DT::renderDataTable(
    DT::datatable(tablephs(), class = 'cell-border stripe',
                  options = list(pageLength = 10, dom = 'Blfrtip', buttons = c('csv'),
                                 lengthMenu = list( c(10, 20, -1), # declare values
                                                    c(10, 20, "All"))),
                  extensions = 'Buttons')
  )
}

shinyApp(ui, server)

