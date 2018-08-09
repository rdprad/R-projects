library(odbc)
library(lubridate)
library(dygraphs)
library(dplyr)
library(xts)
library(forecast)
library(odbc)
library(dygraphs)
con <- dbConnect(odbc::odbc(), "SQLD1")
con_DF <- dbGetQuery(con, "select [HourlyDate],[Tonnes to Mill] FROM MinetoMill2.dbo.reportHourly")

con_DF$HourlyDate <- as.POSIXct(strptime(con_DF$HourlyDate, "%Y-%m-%d %H:%M:%S"))
con_DF$`Tonnes to Mill`<- as.integer(con_DF$`Tonnes to Mill`)
a <-con_DF %>% 
 filter(!is.na(HourlyDate),!is.na(`Tonnes to Mill`) ) %>% 
 arrange(HourlyDate)

a$ma <- ma(a$`Tonnes to Mill`,24)
a$`Tonnes to Mill`<-NULL

dygraph( xts(x = a, order.by = a$HourlyDate) ) %>%
 dyAxis("x", drawGrid = FALSE) %>%
 dyAxis("y", label = "tonnes per hour") %>%
 # dyHighlight(highlightCircleSize = 5, 
 #             highlightSeriesBackgroundAlpha = 0.2,
 #             hideOnMouseOut = FALSE) %>% 
 dyOptions(includeZero = TRUE, 
           colors = "grey",
           axisLineColor = "lightgrey", 
           gridLineColor = "lightgrey") %>% 
 dyLimit(5000, color = "red") %>% 
 dyLimit(6500, color = "red") %>% 
 dyShading(from=5000, to=6500, axis="y", color = "#FFE6E6") %>% 
 dyRangeSelector()

a$diff <- c(NA,diff(a$ma))

class(a$HourlyDate)
library(zoo)
a$to <- ifelse(abs(a$diff)>60, a$HourlyDate, NA)
# a$from <- as.POSIXct(a$from, origin = '1970-01-01') 
# a$to <- ""

for (i in seq_along(a$HourlyDate)) {
  if (!is.na(a$to[i])) {
    a$from[i] <- a$HourlyDate[i-1]
  } else {
    a$from[i] <- NA
  }
}

# as.numeric(a$HourlyDate[1])
a$from <- as.POSIXct(a$from, origin = '1970-01-01')
a$to <- as.POSIXct(as.numeric(a$to), origin = '1970-01-01') 


a$HourlyDate[14]
a$from[14]

dg <- dygraph( xts(x = a, order.by = a$HourlyDate) ) %>%
 dyAxis("x", drawGrid = FALSE) %>%
 dyAxis("y", label = "tonnes per hour") %>%
 # dyHighlight(highlightCircleSize = 5, 
 #             highlightSeriesBackgroundAlpha = 0.2,
 #             hideOnMouseOut = FALSE) %>% 
 dyOptions(includeZero = TRUE, 
           colors = "grey",
           axisLineColor = "lightgrey", 
           gridLineColor = "lightgrey") %>% 
 # dyLimit(5000, color = "red") %>% 
 # dyLimit(6500, color = "red") %>% 
 # dyShading(from=5000, to=6500, axis="y", color = "#FFE6E6") %>% 
 # dyShading(from=a$from, to = a$to, color = "#CCEBD6") %>% 
 dyRangeSelector()

for( i in seq_along(a$HourlyDate) ) {
  dg <- dyShading(dg, from = a$from[i] , to = a$to[i], color="red" )
}

dg


