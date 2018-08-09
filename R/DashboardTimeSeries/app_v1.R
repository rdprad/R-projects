## app.R #
library(shiny)
library(shinydashboard)
library(odbc)
library(dplyr)
library(ggplot2)
library(dygraphs)

# con <- dbConnect(odbc::odbc(), "SQLD1")
# con_DF <- dbGetQuery(con, "select * FROM [OpenPit].[dbo].[RawMMRSActivity] WHERE [Year]=2018")
# con_DF2 <- dbGetQuery(con, "select * FROM [OpenPit].[dbo].[RawMMRSCycleDataByShift] WHERE [Date]>='2018-01-01'")
# con_DF3 <- dbGetQuery(con, "select * FROM [OpenPit].[dbo].[RawMinesightOreControlDataBySource]")
# 
# tmp1 <- con_DF %>%
#   filter(grepl('930', Fleet), grepl('Utilized', `Time Class`), Destination=="CRSH", `Time Code`=="Dump", Month==8) %>%
#   arrange(Date, Shift) %>% 
#   group_by(Date, Shift, Location) %>%
#   summarise(time = sum(`Time Usage`)/60/60)
# 
# tmp2 <- con_DF2 %>% 
#   filter(Destination=="CRSH",grepl('930', Fleet)) %>% 
#   arrange(Date, Shift) %>% 
#   group_by(Date, Shift, Source)%>%
#   summarise(tonnes = sum(`t Load`)) %>% 
#   rename(Location=Source)
# 
# tmp3 <- con_DF3 %>% 
#   select(cutName, BCu, BCu_REC, BAu, BAu_REC, BAg, BAg_REC, BAS, CONAS, SPI, MB, CI, HPKT, NSROC, Phase) %>% 
#   rename(Location = cutName)
# 
# table_df <- merge(tmp1, tmp2, by=c("Date", "Shift", "Location"), all=T)
# table_df <- merge(table_df, tmp3, by=c("Location"))
# table_df <- table_df %>% 
#               select(-(time)) %>% 
#               na.omit() %>% 
#               arrange(Date, Shift)
# 
# table_plot <- table_df %>% 
#   group_by(Date) %>%
#   summarise(tons = sum(tonnes),
#             model_tput = 1000/weighted.mean(HPKT, tonnes),
#             EU = 100,
#             daily_tput = 24*model_tput*EU/100, 
#             Cu = weighted.mean(BCu, tonnes), 
#             Au = weighted.mean(BAu, tonnes), 
#             SPI = weighted.mean(SPI, tonnes))
# table_plot2 <- table_plot %>% 
#   select(tons, Cu, Au)
# 
# table_plot2 <- xts::xts(table_plot2, as.Date(table_plot$Date, "%Y-%m-%d"))

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(disable = FALSE,
                                    title = "Crusher Feed",
                                    dropdownMenu(type = "messages",
                                                 messageItem(
                                                   from = "Sales Dept",
                                                   message = "Sales are steady this month."
                                                 ),
                                                 messageItem(
                                                   from = "New User",
                                                   message = "How do I register?",
                                                   icon = icon("question"),
                                                   time = "13:45"
                                                 ),
                                                 messageItem(
                                                   from = "Support",
                                                   message = "The new server is ready.",
                                                   icon = icon("life-ring"),
                                                   time = "2014-12-01"
                                                 )
                                    ),
                                    dropdownMenu(type = "notifications",
                                                 notificationItem(
                                                   text = "5 new users today",
                                                   icon("users")
                                                 ),
                                                 notificationItem(
                                                   text = "12 items delivered",
                                                   icon("truck"),
                                                   status = "success"
                                                 ),
                                                 notificationItem(
                                                   text = "Server load at 86%",
                                                   icon = icon("exclamation-triangle"),
                                                   status = "warning"
                                                 )
                                    ),
                                    dropdownMenu(type = "tasks", badgeStatus = "success",
                                                 taskItem(value = 90, color = "green",
                                                          "Documentation"
                                                 ),
                                                 taskItem(value = 17, color = "aqua",
                                                          "Project X"
                                                 ),
                                                 taskItem(value = 75, color = "yellow",
                                                          "Server deployment"
                                                 ),
                                                 taskItem(value = 80, color = "red",
                                                          "Overall project"
                                                 )
                                    )
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Widgets", tabName = "widgets", icon = icon("th"),
                                 badgeLabel = "rdp", badgeColor = "green")
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
        # /* logo */
        # .skin-black .main-header .logo {
        #                       background-color: grey;
        #                       }
        # 
        # /* logo when hovered */
        # .skin-black .main-header .logo:hover {
        #                       background-color: grey;
        #                       }
        # 
        # /* navbar (rest of the header) */
        # .skin-black .main-header .navbar {
        #                       background-color: grey;
        #                       }
                       "))
                      ),
                      tabItems(
                        # First tab content
                        tabItem(tabName = "dashboard",
                                # infoBoxes with fill=TRUE
                                fluidRow(
                                  valueBox("Cu", 10 * 2, icon = icon("credit-card"), width = 2),
                                  infoBoxOutput("progressBox2"),
                                  infoBoxOutput("approvalBox2")
                                ),
                                
                                fluidRow(
                                  box(title = "YTD Ore Feed to Crusher (MMRS)", width = 12, status = "warning", solidHeader = TRUE,
                                      dygraphOutput("plot", height = 200))
                                ),
                                
                                fluidRow(
                                  box(width = 4,
                                      title = "Histogram", status = "primary", solidHeader = TRUE,
                                      collapsible = FALSE,
                                      plotOutput("plot3", height = 235)
                                  ),
                                  
                                  box(width = 4,
                                      title = "Inputs", status = "warning", solidHeader = TRUE,
                                      "Box content here", br(), "More box content",
                                      sliderInput("slider", "Slider input:", 1, 100, 50), height = 300,
                                      textInput("text", "Text input:")
                                  ),
                                  
                                  tabBox(
                                    title = "First tabBox",
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset1", height = "250px",
                                    tabPanel("Tab1", "First tab content"),
                                    tabPanel("Tab2", "Tab content 2")
                                  ),
                                  tabBox(
                                    side = "right", height = "250px",
                                    selected = "Tab3",
                                    tabPanel("Tab1", "Tab content 1"),
                                    tabPanel("Tab2", "Tab content 2"),
                                    tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                                  )
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
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot3 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  
  # Plot Dygraphs for crusher feed
  output$plot <- renderDygraph({
    dygraph(table_plot2, group = "CrusherOut")%>% 
      dyAxis("y", valueRange = c(0, 170000), independentTicks = TRUE)%>%
      dyAxis("y2", valueRange = c(0, 1.2), independentTicks = TRUE) %>%
      dyFilledLine('tons', stepPlot = TRUE) %>%
      dySeries("Cu", axis=('y2'), color = "blue") %>% 
      dySeries("Au", axis=('y2'), color = "orange") %>% 
      dyRangeSelector() %>% 
      dyCrosshair(direction = "vertical") %>% 
      dyHighlight(highlightCircleSize = 3, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE) %>% 
      dyLegend(show = "follow") %>% 
      dyOptions(drawGrid = FALSE)
  })
}

shinyApp(ui, server)
