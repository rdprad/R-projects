## app.R #
library(shiny)
library(shinydashboard)
library(odbc)
library(dplyr)
library(ggplot2)
library(dygraphs)
library(dbplyr)
library(odbc)
library(DBI)
library(rCharts)
library(shinyWidgets)
library(formattable)
library(tidyr)

# dw <- config::get("datawarehouse")
# dw
# 
# con <- DBI::dbConnect(odbc::odbc(),
#    Driver = dw$driver,
#    Server = dw$server,
#    # UID    = dw$uid,
#    # PWD    = dw$pwd,
#    # Port   = dw$port,
#    Database = dw$database
# )

con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};SERVER=MNOYTSQLD1;database=OpenPit;trusted_connection=true")
then <- Sys.Date()-7
now <- Sys.Date()
year <- substr(Sys.Date(),1,4)
month <- as.numeric(substr(Sys.Date(),6,7))
q1 <- tbl(con, "Actual_Data") %>% 
          # filter(Year==year, Date >= then, Date <=now, `Major Destination` =="CRSH") 
          filter(Year==year, `Major Destination` =="CRSH") 

days <- data.frame(Date = seq(as.Date(paste0(year,"-01-01")),now,1))
days$Date <- as.character(days$Date)
SW <- c("1","2","3","4","42","5","TK","HG1","MG1","MG4","LG1")
Qcon <- collect(q1)
YTD_df <- Qcon %>% 
  mutate(OreType = ifelse(`Minor Source` %in% SW, "SW Ore", "Central Ore")) %>% 
  group_by(Date, Source, `Minor Source`, OreType) %>%
  # group_by(Year) %>% 
  summarise(Mass    = sum(Tonnes),
            Cu      = weighted.mean(CU, Tonnes),
            Au      = weighted.mean(AU, Tonnes),
            RecCu   = weighted.mean(RECCU, Tonnes),
            RecAu   = weighted.mean(RECAU, Tonnes),
            Cu_rec  = RecCu/Cu*100,
            Au_rec  = RecAu/Au*100, 
            SPI     = weighted.mean(SPI, Tonnes),
            MB      = weighted.mean(MB, Tonnes),
            CI      = weighted.mean(CI, Tonnes),
            HPKT    = weighted.mean(HPKT, Tonnes),
            TPOH    = 1000/HPKT,
            AS      = weighted.mean(AS, Tonnes),
            CONAS   = weighted.mean(CONAS, ConTon))
YTD_df <- merge(YTD_df, days, by=c("Date"), all=T)


#YTD 1
dygraph_YTD <- merge(YTD_df %>% 
                       group_by(Date) %>% 
                       filter(OreType %in% "Central Ore") %>% 
                       summarise(Central_Ore = round(sum(Mass),0)),
                     YTD_df %>% 
                       group_by(Date) %>% 
                       summarise(Total_Feed = format(round(sum(Mass),0))),
                     by=c("Date"), all=T)
dygraph_YTD$Date <- as.Date(dygraph_YTD$Date)
dygraph_YTD <- xts::xts(dygraph_YTD,dygraph_YTD$Date)

#YTD 2
dygraph_YTD2 <- Qcon %>%
  group_by(Date) %>%
  summarise(Cu      = round(weighted.mean(CU, Tonnes),3),
            Au      = round(weighted.mean(AU, Tonnes),3))
dygraph_YTD2 <- merge(dygraph_YTD2, days, by=c("Date"), all=T)
dygraph_YTD2$Date <- as.Date(dygraph_YTD2$Date)
dygraph_YTD2 <- xts::xts(dygraph_YTD2,dygraph_YTD2$Date)


#PI query
Query_PI <- tbl(con, "RawOPSKPI") %>% 
  filter(AttributeName %in% c("Milled_Ore_DMT_Daily_Official","Milled_Ore_Cu_Daily_Official",
                              "Milled_Ore_Au_Daily_Official","Milled_Ore_EU_Daily","Fin_Rec_Cu_Daily_Official",
                              "Fin_Rec_Au_Daily_Official","Col_Con_As_Daily_Official","CRU_Ore_DMT_Daily_Official",
                              "Milled_Ore_TPOH_Daily_Official", "Col_Con_DMT_Daily"))
Query_PI_DF <- collect(Query_PI)
Query_PI_DF$Date <- as.Date(Query_PI_DF$ValueDateTime)


ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(disable = FALSE,
                                    title = "Crusher Feed",
                                    dropdownMenu(type = "messages",
                                                 messageItem(
                                                   from = "Need More?",
                                                   message = "contact: Rama Pradnyana",
                                                   icon = icon("question"),
                                                   time = "Aug 2018"
                                                 )
                                    ),
                                    dropdownMenu(type = "notifications",
                                                 notificationItem(
                                                   text = "access to MNOYTSQLD1 is required",
                                                   icon("users"),
                                                   status = "warning"
                                                 )
                                    )
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        # dateInput("date", label = "", value = Sys.Date()-1),
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Widgets", tabName = "widgets", icon = icon("th")),
                        actionBttn(
                          inputId = "rfrshSQL",
                          label = "refreshSQL",
                          style = "jelly", 
                          color = "primary",
                          icon = icon("refresh"), 
                          size = "xs"
                        )
   
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$style(HTML("
                         .content-wrapper {
                           background-color: white !important;
                         }
                         .main-sidebar {
                           background-color: black !important;
                         }
        /* logo */
        .skin-yellow .main-header .logo {
                              background-color: black;
                              }

        /* logo when hovered */
        .skin-yellow .main-header .logo:hover {
                              background-color: black;
                              }

        /* navbar (rest of the header) */
        .skin-yellow .main-header .navbar {
                              background-color: black;
                              }
                       "))
                      ),
                      tabItems(
                        # First tab content
                        tabItem(tabName = "dashboard",
                                # fluidRow(box(width=2, dateInput("date", label = "", value = Sys.Date()))),
                                # fluidRow(box(width=12,title = "Crusher Feed (MMRS Record & Mine Model)",status = "warning", solidHeader = F,
                                #              valueBoxOutput("ton_mmrs", width = 2),
                                #              valueBoxOutput("cu_mmrs", width = 2),
                                #              valueBoxOutput("au_mmrs", width = 2),
                                #              valueBoxOutput("spi_mmrs", width = 2),
                                #              valueBoxOutput("tpoh_mmrs", width = 2),
                                #              valueBoxOutput("conas_mmrs", width = 2))),

                                fluidRow(
                                  column(width = 12,
                                         fluidRow(
                                           column(width = 4, 
                                                  fluidRow(
                                                    column(width = 6, dateRangeInput("date", label = h4("Date range"), 
                                                                                     start = Sys.Date()-1, end = Sys.Date()-1)),
                                                    column(width = 12,h4("Ore Type Proportion"),
                                                           showOutput("chart_pie_1", 'highcharts'),
                                                           HTML('<style>.rChart {width: 100%}</style>'))
                                                    )),
                                           column(width = 8,
                                                  fluidRow(
                                                    box(title = "Daily Source Detail (MMRS Record & Mine Model)",width = 12,
                                                        status = "warning", solidHeader = F,
                                                        formattableOutput("table1"),
                                                        formattableOutput("table2")),
                                                    box(title = "MMRS vs. PI Comparison", width = 12, 
                                                        status = "warning", solidHeader = F,
                                                        formattableOutput("table3"),
                                                        tags$i(h6("Note: MMRS 'mill' number above is calculated from modeled TPOH (from SPI, MB, CI) and multiplied with actual mill EU")))
                                                  )
                                           )
                                         )
                                  )
                                ),
                                fluidRow(
                                  box(title = "YTD Ore Feed to Crusher (MMRS)", width = 12, status = "warning", solidHeader = F,
                                      dygraphOutput("plot_ytd", height = 150),
                                      dygraphOutput("plot_ytd2", height = 150))
                                )
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "widgets",
                                h2("Nothing to see here yet, go back to Dashboard")
                        )
                      )
                    )
)

server <- function(input, output) {
  observeEvent(input$rfrshSQL,{
    source(here::here("objects","refresh.R"))
  })
  
  # Plot Dygraphs for crusher feed
  output$plot_ytd <- renderDygraph({
    dygraph(dygraph_YTD, group = "CrusherOut")%>%
      dyAxis("y", valueRange = c(0, 170000), independentTicks = TRUE)%>%
      # dyRangeSelector() %>%
      dyFilledLine('Total_Feed', stepPlot = TRUE, color = 'rgba(250, 137, 25, 1)') %>%
      dyFilledLine('Central_Ore', stepPlot = TRUE, color = 'rgba(25, 137, 250, 1)') %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 3,
                  highlightSeriesBackgroundAlpha = 0.4,
                  hideOnMouseOut = TRUE) %>%
      dyLimit(110000, color = "red","110k") %>% 
      dyLimit(130000, color = "green", "130k") %>% 
      dyLegend(show = "onmouseover", width = 400) %>%
      dyOptions(drawGrid = FALSE, fillGraph = T) #%>% 
      # dyLegend(show = "follow")
  })
  
  # Plot Dygraphs for Cu/Au grade
  output$plot_ytd2 <- renderDygraph({
    dygraph(dygraph_YTD2, group = "CrusherOut")%>%
      dyAxis("y", valueRange = c(0, 1.2), independentTicks = TRUE)%>%
      dySeries('Cu', color = "blue",strokeWidth = 0.5,drawPoints = TRUE, pointSize = 1) %>%
      dySeries('Au', color = "orange",strokeWidth = 0.5,drawPoints = TRUE, pointSize = 1) %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 3,
                  highlightSeriesBackgroundAlpha = 0.4,
                  hideOnMouseOut = TRUE) %>%
      dyLegend(show = "onmouseover", width = 400) %>%
      dyOptions(drawGrid = F, fillGraph = F) #%>% 
      # dyLegend(show = "follow")
  })
  
  
  # output$datepick <- renderPrint({ input$date })
  
  table_daily_tot <- reactive({
    Qcon %>% 
      filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
      mutate(OreType = ifelse(`Minor Source` %in% SW, "SW Ore", "Central Ore")) %>% 
      group_by(Date) %>%
      summarise(Mass    = round(sum(Tonnes),0),
                Cu      = round(weighted.mean(CU, Tonnes),3),
                Au      = round(weighted.mean(AU, Tonnes),3),
                RecCu   = round(weighted.mean(RECCU, Tonnes),3),
                RecAu   = round(weighted.mean(RECAU, Tonnes),3),
                Cu_rec  = round(RecCu/Cu*100,1),
                Au_rec  = round(RecAu/Au*100,1), 
                SPI     = round(weighted.mean(SPI, Tonnes),0),
                MB      = round(weighted.mean(MB, Tonnes),0),
                CI      = round(weighted.mean(CI, Tonnes),0),
                HPKT    = weighted.mean(HPKT, Tonnes),
                TPOH    = round(1000/HPKT,0),
                AS      = round(weighted.mean(AS, Tonnes),2),
                CONAS   = round(weighted.mean(CONAS, ConTon),0))
  })
  
  table_daily_detail <- reactive({
    Qcon %>% 
      filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
      mutate(OreType = ifelse(`Minor Source` %in% SW, "SW Ore", "Central Ore")) %>% 
      group_by(Date, Source, `Minor Source`, OreType) %>%
      summarise(Mass    = round(sum(Tonnes),0),
                Cu      = round(weighted.mean(CU, Tonnes),3),
                Au      = round(weighted.mean(AU, Tonnes),3),
                RecCu   = round(weighted.mean(RECCU, Tonnes),3),
                RecAu   = round(weighted.mean(RECAU, Tonnes),3),
                Cu_rec  = round(RecCu/Cu*100,1),
                Au_rec  = round(RecAu/Au*100,1), 
                SPI     = round(weighted.mean(SPI, Tonnes),0),
                MB      = round(weighted.mean(MB, Tonnes),0),
                CI      = round(weighted.mean(CI, Tonnes),0),
                HPKT    = weighted.mean(HPKT, Tonnes),
                TPOH    = round(1000/HPKT,0),
                AS      = round(weighted.mean(AS, Tonnes),2),
                CONAS   = round(weighted.mean(CONAS, ConTon),0))
  })
  
  
  output$ton_mmrs <- renderValueBox({
    valueBox(
      "Tonnes",
      paste0(format(table_daily_tot()$Mass, big.mark = ",")," tonnes"), icon = icon("line-chart"), color = "yellow"
    )
  })
  
  output$cu_mmrs <- renderValueBox({
    valueBox(
      "Cu",
      paste0(format(table_daily_tot()$Cu, big.mark = ",")," %"), icon = icon("percent"), color = "yellow"
    )
  })
  
  output$au_mmrs <- renderValueBox({
    valueBox(
      "Au",
      paste0(format(table_daily_tot()$Au, big.mark = ",")," g/t"), icon = icon("percent"), color = "yellow"
    )
  })
  
  output$spi_mmrs <- renderValueBox({
    valueBox(
      "SPI",
      paste0(format(table_daily_tot()$SPI, big.mark = ",")," min"), icon = icon("area-chart"), color = "yellow"
    )
  })
  
  output$tpoh_mmrs <- renderValueBox({
    valueBox(
      "TPOH",
      paste0(format(table_daily_tot()$TPOH, big.mark = ",")," t/hr"), icon = icon("line-chart"), color = "yellow"
    )
  })
  
  output$conas_mmrs <- renderValueBox({
    valueBox(
      "ConAs",
      paste0(format(table_daily_tot()$CONAS, big.mark = ",")," ppm"), icon = icon("percent"), color = "yellow"
    )
  })
  
  
  table_daily_pie_1 <- reactive({
    Qcon %>% 
      filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
      mutate(OreType = ifelse(`Minor Source` %in% SW, "SW Ore", "Central Ore")) %>% 
      group_by(OreType) %>%
      summarise(Mass    = round(sum(Tonnes),0))
  })
  
  table_daily_column_1 <- reactive({
    Qcon %>%
      filter(Date >= input$date[1] & Date <= input$date[2]) %>%
      mutate(OreType = ifelse(`Minor Source` %in% SW, "SW Ore", "Central Ore")) %>%
      group_by(OreType,`Minor Source`, Source) %>%
      summarise(Mass    = accounting(round(sum(Tonnes),0),format = "d"),
                Cu      = round(weighted.mean(CU, Tonnes),3),
                Au      = round(weighted.mean(AU, Tonnes),3),
                Cu_rec  = round(weighted.mean(RECCU, Tonnes)/Cu*100,1),
                Au_rec  = round(weighted.mean(RECAU, Tonnes)/Au*100,1), 
                SPI     = accounting(weighted.mean(SPI, Tonnes),format = "d"),
                MB      = accounting(weighted.mean(MB, Tonnes),format = "d"),
                CI      = accounting(weighted.mean(CI, Tonnes),format = "d"),
                TPOH    = accounting(1000/weighted.mean(HPKT, Tonnes), format = "d"),
                AS      = accounting(weighted.mean(AS, Tonnes),format = "d"),
                CONAS   = accounting(weighted.mean(CONAS, ConTon),format = "d"))
  })
  
  table_daily_column_2 <- reactive({
    Qcon %>%
      filter(Date >= input$date[1] & Date <= input$date[2]) %>%
      mutate(OreType = "__TOTAL__",
             `Minor Source 2` = "__TOTAL__",
             Source2 = "__ALL SOURCES__") %>%
      group_by(OreType, `Minor Source 2`, Source2) %>%
      summarise(Mass    = accounting(round(sum(Tonnes),0),format = "d"),
                Cu      = round(weighted.mean(CU, Tonnes),3),
                Au      = round(weighted.mean(AU, Tonnes),3),
                Cu_rec  = round(weighted.mean(RECCU, Tonnes)/Cu*100,1),
                Au_rec  = round(weighted.mean(RECAU, Tonnes)/Au*100,1), 
                SPI     = accounting(weighted.mean(SPI, Tonnes),format = "d"),
                MB      = accounting(weighted.mean(MB, Tonnes),format = "d"),
                CI      = accounting(weighted.mean(CI, Tonnes),format = "d"),
                TPOH    = accounting(1000/weighted.mean(HPKT, Tonnes), format = "d"),
                AS      = accounting(weighted.mean(AS, Tonnes),format = "d"),
                CONAS   = accounting(weighted.mean(CONAS, ConTon),format = "d")) %>% 
      rename(`Minor Source` = `Minor Source 2`) %>% 
      rename(Source = Source2)
  })
  
  output$table1 <- renderFormattable({
    formattable(table_daily_column_1(), list(
      area(col = Mass) ~ normalize_bar("lightgreen", 0.2),
      Cu       = color_tile("white", "lightblue"),
      Cu_rec   = color_tile("white", "lightblue"),
      Au       = color_tile("white", "orange"),
      Au_rec   = color_tile("white", "orange"),
      TPOH     = color_tile("white", "lightgreen"),
      AS       = color_tile("white", "pink"),
      CONAS    = color_tile("white", "pink"),
      OreType  = formatter("span", style = x ~ ifelse(x == "Central Ore", 
                                                      style(color = 'rgba(25, 137, 250, 1)', font.weight = "bold"),
                                                      style(color = 'rgba(250, 137, 25, 1)', font.weight = "bold")))
    ))
  })
  
  output$table2 <- renderFormattable({
    formattable(table_daily_column_2()
    )
  })
  
  output$chart_pie_1 <- renderChart2({
    x = table_daily_pie_1()
    p1 = hPlot(x = "OreType", y = "Mass", data = x, type = "pie")
    p1$colors('rgba(25, 137, 250, 0.8)','rgba(250, 137, 25, 0.8)')
    return(p1)
  })
  
  table_EU <- reactive({
    Query_PI_DF %>%
      filter(Date >= input$date[1] & Date <= input$date[2]) %>%
      spread(AttributeName, AttributeValue) %>%
      select(Milled_Ore_TPOH_Daily_Official,Milled_Ore_DMT_Daily_Official) %>%  
      mutate(Mill_OPhrs = Milled_Ore_DMT_Daily_Official/Milled_Ore_TPOH_Daily_Official)
  })
  
  table_daily_comparison <- reactive({
    rbind(Qcon %>% 
            filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
            mutate(Remarks = "MMRS Record & Mine Model") %>% 
            group_by(Remarks) %>%
            summarise(Crusher = accounting(sum(Tonnes),format = "d"),
                      Mill    = accounting(sum(table_EU()$Mill_OPhrs)*1000/weighted.mean(HPKT, Tonnes),format = "d"),
                      Cu      = round(weighted.mean(CU, Tonnes),3),
                      Au      = round(weighted.mean(AU, Tonnes),3),
                      Cu_rec  = round(weighted.mean(RECCU, Tonnes)/Cu*100,1),
                      Au_rec  = round(weighted.mean(RECAU, Tonnes)/Au*100,1),
                      TPOH    = accounting(1000/weighted.mean(HPKT, Tonnes),format = "d"),
                      CONAS   = accounting(weighted.mean(CONAS, ConTon),format = "d")),
          Query_PI_DF %>% 
            filter(Date >= input$date[1] & Date <= input$date[2]) %>% 
            spread(AttributeName, AttributeValue) %>% 
            mutate(Remarks  = "PI Actual Data",
                   metal_Cu = Milled_Ore_Cu_Daily_Official*Milled_Ore_DMT_Daily_Official,
                   metal_Au = Milled_Ore_Au_Daily_Official*Milled_Ore_DMT_Daily_Official) %>% 
            group_by(Remarks) %>% 
            summarise(Crusher = accounting(sum(CRU_Ore_DMT_Daily_Official),format = "d"),
                      Mill    = accounting(sum(Milled_Ore_DMT_Daily_Official),format = "d"),
                      Cu      = round(weighted.mean(Milled_Ore_Cu_Daily_Official, Milled_Ore_DMT_Daily_Official),3),
                      Au      = round(weighted.mean(Milled_Ore_Au_Daily_Official, Milled_Ore_DMT_Daily_Official),3),
                      Cu_rec  = round(weighted.mean(Fin_Rec_Cu_Daily_Official, metal_Cu),1),
                      Au_rec  = round(weighted.mean(Fin_Rec_Au_Daily_Official, metal_Au),1),
                      TPOH    = accounting(Mill/sum(table_EU()$Mill_OPhrs),format = "d"),
                      CONAS   = accounting(weighted.mean(Col_Con_As_Daily_Official, Col_Con_DMT_Daily),format = "d"))
    )
  })
  
  output$table3 <- renderFormattable({
    formattable(table_daily_comparison(), list(
      Crusher = color_tile("pink", "white"),
      Mill    = color_tile("pink", "white"),
      Cu      = color_tile("pink", "white"),
      Au      = color_tile("pink", "white"),
      Cu_rec  = color_tile("pink", "white"),
      Au_rec  = color_tile("pink", "white"),
      CONAS   = color_tile("pink", "white"),
      TPOH    = color_tile("pink", "white")
    )
    )
  })
  
}

shinyApp(ui, server)
