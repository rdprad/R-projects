library(odbc)
con <- dbConnect(odbc::odbc(), "SQLD1")
con_DF <- dbGetQuery(con, "select * FROM [OpenPit].[dbo].[RawMMRSActivity] WHERE [Year]=2018")
con_DF2 <- dbGetQuery(con, "select * FROM [OpenPit].[dbo].[RawMMRSCycleDataByShift] WHERE [Date]>='2018-01-01'")
con_DF3 <- dbGetQuery(con, "select * FROM [OpenPit].[dbo].[RawMinesightOreControlDataBySource]")

library(dplyr)
library(ggplot2)
tmp1 <- con_DF %>%
  filter(grepl('930', Fleet), grepl('Utilized', `Time Class`), Destination=="CRSH", `Time Code`=="Dump", Month==8) %>%
  arrange(Date, Shift) %>% 
  # group_by(Date) %>% 
  group_by(Date, Shift, Location) %>%
  summarise(time = sum(`Time Usage`)/60/60)
  # ggplot()+
  # geom_histogram(aes(`Time Usage`), binwidth = 10, alpha=0.5)+
  # geom_density(aes(`Time Usage`), color="red")


tmp2 <- con_DF2 %>% 
  filter(Destination=="CRSH",grepl('930', Fleet)) %>% 
  arrange(Date, Shift) %>% 
  group_by(Date, Shift, Source)%>%
  summarise(tonnes = sum(`t Load`)) %>% 
  rename(Location=Source)

tmp3 <- con_DF3 %>% 
  select(cutName, BCu, BCu_REC, BAu, BAu_REC, BAg, BAg_REC, BAS, CONAS, SPI, MB, CI, HPKT, NSROC, Phase) %>% 
  rename(Location = cutName)

table_df <- merge(tmp1, tmp2, by=c("Date", "Shift", "Location"), all=T)
table_df <- merge(table_df, tmp3, by=c("Location"))

table_df <- table_df %>% 
              select(-(time)) %>% 
              na.omit() %>% 
              arrange(Date, Shift)
table_df %>% 
  filter(substr(Date,1,7) == "2018-07") %>%
  # filter(Date == "2018-08-01") %>%
  # group_by(Date) %>%
  summarise(tons = sum(tonnes),
            model_tput = 1000/weighted.mean(HPKT, tonnes),
            EU = 100,
            daily_tput = 24*model_tput*EU/100, 
            Cu = weighted.mean(BCu, tonnes), 
            Au = weighted.mean(BAu, tonnes),
            Cu_rec = weighted.mean(BCu_REC,tonnes)/Cu*100,
            Au_rec = weighted.mean(BAu_REC,tonnes)/Au*100,
            SPI = weighted.mean(SPI, tonnes))

table_plot <- table_df %>% 
  # filter(substr(Date,1,7) == "2018-07") %>%
  group_by(Date) %>%
  summarise(tons = sum(tonnes),
            model_tput = 1000/weighted.mean(HPKT, tonnes),
            EU = 100,
            daily_tput = 24*model_tput*EU/100, 
            Cu = weighted.mean(BCu, tonnes), 
            Au = weighted.mean(BAu, tonnes), 
            SPI = weighted.mean(SPI, tonnes))
# table_plot$Date <- as.POSIXct(strftime(table_plot$Date, "%Y-%m-%d"))
# class(table_plot$Date2)
table_plot2 <- table_plot %>% 
  select(tons, Cu, Au)

table_plot2 <- xts::xts(table_plot2, as.Date(table_plot$Date, "%Y-%m-%d"))

dygraph(table_plot2, main = "Ore Feed to Crusher", group = "CrusherOut")%>% 
  dyAxis("y", label = "Crusher Feed", valueRange = c(0, 170000), independentTicks = TRUE)%>%
  dyAxis("y2", label = "Cu/Au Head Grade", valueRange = c(0, 1.2), independentTicks = TRUE) %>%
  dyFilledLine('tons', stepPlot = TRUE) %>%
  # dyBarSeries('tons') %>% 
  dySeries("Cu", axis=('y2'), color = "blue") %>% 
  dySeries("Au", axis=('y2'), color = "orange") %>% 
  dyRangeSelector() %>% 
  dyCrosshair(direction = "vertical") %>% 
  dyHighlight(highlightCircleSize = 3, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>% 
  dyLegend(show = "follow") %>% 
  dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)


# Extract data from dyrangeselector to shiny------------
# library(shiny)
# library(dygraphs)
# library(dplyr)
# library(stringr)
# 
# indoConc <- Indometh[Indometh$Subject == 1, c("time", "conc")]
# takeLastnvalues <- -3
# ui <- fluidPage(dygraphOutput("plot"),tableOutput("table"))
# 
# server <- shinyServer(function(input, output,session) {
# 
#   values <- reactiveValues()  
#   observeEvent(input$plot_date_window,{
#     value1 <- input$plot_date_window[[1]]
#     value2 <- input$plot_date_window[[2]]
#     value1 <- sub("Z", "", value1)
#     value2 <- sub("Z", "", value2)
#     value1 <- str_sub(value1,takeLastnvalues,-1)
#     value2 <- str_sub(value2,takeLastnvalues,-1)
#     values$v1 <- as.numeric(value1)
#     values$v2 <- as.numeric(value2)
#   })
# 
#   output$plot <- renderDygraph({
#     indoConc %>%
#       dygraph %>%
#       dyRangeSelector
#   })
# 
#   output$table <- renderTable({
#     indoConc %>%
#       filter(time >= min(values$v1), time <= max(values$v2)) %>%
#       summarise(total_conc = sum(conc))
#   })
# })
# 
# shinyApp(ui, server)

