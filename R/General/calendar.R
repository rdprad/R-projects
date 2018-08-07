# Example from the web----------

library(ggplot2)
library(plyr)
library(scales)
library(zoo)

df <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/yahoo.csv")
df$date <- as.Date(df$date)  # format date
df <- df[df$year >= 2012, ]  # filter reqd years

# Create Month Week
df$yearmonth <- as.yearmon(df$date)
df$yearmonthf <- factor(df$yearmonth)
df <- ddply(df,.(yearmonthf), transform, monthweek=1+week-min(week))  # compute week number of month
df[,"year"]
df <- df[, c("year", "yearmonthf", "monthf", "week", "monthweek", "weekdayf", "VIX.Close")]
head(df)
is.factor(df$monthf)
df$monthf <- factor(df$monthf, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

library(ggiraph)
tmp <- subset(df, year==2016)
a<- ggplot(tmp, aes(monthweek, weekdayf, fill = VIX.Close)) + 
  geom_tile_interactive(tooltip=paste("test", tmp$weekdayf, sep=" "),colour = "white")+
  coord_equal(ratio=1)+
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="grey", high="green") +
  # labs(#x="Week of Month",
  # #      y="",
  # #      title = "Time-Series Calendar Heatmap", 
  # #      subtitle="Yahoo Closing Price", 
  #      fill="Close")+
  guides(fill=FALSE)+
  labs(x="",y="")

ggiraph(code = {print(a)})


# Create dataframe for 2018 calendar------------

cal_df <- data.frame(dates = seq(as.Date("2019-01-01"), as.Date("2019-12-31"),1))
cal_df$weeknum <- strftime(cal_df$dates, "%V")
cal_df$dayname <- weekdays(cal_df$dates)
cal_df$dayname <- factor(cal_df$dayname,levels = c("Saturday", "Friday", "Thursday", 
                                                   "Wednesday", "Tuesday","Monday","Sunday"))
cal_df$monthname <- months(cal_df$dates)
cal_df$monthname <- factor(cal_df$monthname, levels = unique(cal_df$monthname))
levels(cal_df$monthname)
cal_df$monthweeks <- stringi::stri_datetime_fields(cal_df$dates)$WeekOfMonth
# cara kedua menemukan minggu ke berapa dari tiap bulan
# cal_df$monthweeks <- ceiling(as.numeric(format(cal_df$dates, "%d"))/7)

# Another way
# The wday component of a POSIXlt object is the numeric weekday (0-6 starting on Sunday).
as.POSIXlt(cal_df$dates)$wday
# which you could use to subset a character vector of weekday names
c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
    "Friday", "Saturday")[as.POSIXlt(cal_df$dates)$wday+1]


library(ggplot2)
library(dplyr)
library(ggiraph)
#plot1
cal_df %>% 
 ggplot()+
 geom_tile(aes(x=weeknum, y = dayname), fill="grey", width = 0.85, height=0.85)+
 coord_fixed(ratio=1)+
 guides(fill=FALSE)+
 labs(x="",y="")+
 theme_minimal()

#plot2
tooltip_css <- "background-color:transparent;"
gga <- cal_df %>% 
 ggplot()+
 geom_tile_interactive(aes(x=monthweeks, y = dayname, fill=value),
                       tooltip = paste(format(cal_df$dates,"%d %b"), "<br>",cal_df$comment),
                       width = 0.95, height=0.95)+
 scale_fill_manual(values = c("grey", "red"))+
 facet_wrap(.~monthname, nrow = 2)+
 coord_fixed(ratio=1)+
 # guides(fill=FALSE)+
 labs(x="",y="")+
 theme_minimal()+
 labs(x="",y="")+
 theme(axis.title.x=element_blank(),
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank(),
       panel.grid =element_blank(),
       panel.spacing=unit(0, "lines"))
ggiraph(code = print(gga))
# ggiraph(code = print(gga), tooltip_extra_css = tooltip_css,
#         hover_css = "cursor:pointer;fill:red")

# fill all events from this lines---
# format for date is YYYY-MM-DD
cal_df$comment <- "" # Erase all comments
cal_df[grep("2019-03-05", cal_df$dates),"comment"] <- paste("testing comment")
cal_df[(grep("2019-05-08", cal_df$dates):grep("2019-07-14", cal_df$dates)),"comment"] <- "SH002 Major PM"


# STOP
cal_df$value <- ifelse(cal_df$comment!="",9,1)
cal_df$value <- factor(cal_df$value)


