# Read Ore Control Database
library(RODBC)
odbcDataSources(type = c("all", "user", "system"))
test <-odbcConnect(dsn="MNOYTSQL7")
sqlTables(test, errors = FALSE, as.is = TRUE, schema = "dbo")
test2 <- sqlQuery(test, "select * from OC_AllReady_FRONT")

library(dplyr)
test3 <- test2 %>% 
          select(cutName,BCu,BAu,BAg,BCu_REC,BAu_REC,BAg_REC,SPI,MB,CI,HPKT,Phase)
test3$ORIGIN <- test3$cutName  

# Read file from MMRS dump
dump<-read.csv(file.choose(), header = T)

dump$newtime <- paste(dump$DATEOP,dump$DATETIMEDUMPSTART)
library(lubridate)
dump$newtime2 <- strftime(parse_date_time(as.character(dump$newtime), "%d-%m-%Y %H:%M:%S"), as.POSIXlt)
dump$newtime2 <- dump$newtime2-(8*60*60)
dump$newtime3 <- strftime(dump$newtime2, "%Y-%m-%d %H")

lookuptable1 <- dump %>%
  filter(DESTINATION=="CRSH")

lookuptable2 <- aggregate(REPORTWEIGHT2 ~ newtime3, lookuptable1, sum)
colnames(lookuptable2)[2] <- "TOTALCRS"

dump <- merge(dump,lookuptable2)
dump <- merge(dump, test3)

dump$hovertext <- paste("Summary (by rama :)): ","<br>","<br>",
                        dump$ORIGIN, "<br>",
                        "Phase: ",dump$Phase, "<br>",
                        "Material: ",dump$MATERIAL, "<br>",
                        "Cu: ",round(dump$BCu,2), "<br>",
                        "Au: ",round(dump$BAu,2), "<br>",
                        "SPI: ",round(dump$SPI,0), "<br>",
                        "Tput model: ",round(1000/dump$HPKT,0))

# Other multi plot in one slider
library(plotly)
dump %>%
  filter(DESTINATION=='CRSH') %>% 
  plot_ly(x=~newtime2, text=~hovertext, hoverinfo=~"text") %>% 
  add_lines(y=~TOTALCRS, color =I("blue"), name="hourly total tonnes to Crusher") %>%
  # add_lines(y=~PAYLOADWEIGHT, color =I("red"), name="tonnes per truck to Crusher") %>%
  layout(title = 'Crusher Timeline',
         xaxis = list(title = "Time", showgrid = T,rangeslider = list(type = "date")),
         yaxis = list(title = 'Crusher tonnage (t/hr)', showgrid = T, range=c(0,8000)))

         


