# weekly data for truck speed
# total data extracted is 14 shifts, hence creating 14 data frames and bind it in here
rm(TruckSpeed)

folder <- "C:/Rama/R/TruckSpeedData/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and rbind them into a data frame called data 
TruckSpeed <-   do.call("rbind", 
                  lapply(file_list, 
                    function(x) 
                      read.csv(paste(folder, x, sep=''), 
                            stringsAsFactors = FALSE)))

#convert mph to kph
TruckSpeed$kph <- TruckSpeed$speed * 3.6

#speed def
speedrange1 = 0
speedrange2 = 12.5
speedrange3 = 17.5
speedrange4 = 22.5
speedrange5 = 27.5
speedrange6 = 32.5
speedrange7 = 37.5
speedrange8 = 42.5
speedlegend1 =paste(speedrange1,"-",speedrange2,"kph")
speedlegend2 =paste(speedrange2,"-",speedrange3,"kph")
speedlegend3 =paste(speedrange3,"-",speedrange4,"kph")
speedlegend4 =paste(speedrange4,"-",speedrange5,"kph")
speedlegend5 =paste(speedrange5,"-",speedrange6,"kph")
speedlegend6 =paste(speedrange6,"-",speedrange7,"kph")
speedlegend7 =paste(speedrange7,"-",speedrange8,"kph")
speedlegend8 =paste(">",speedrange8,"kph")

# Change option between EMPTY or LOADED (0 for empty and 1 for loaded)
status = 0
TruckStatus1 <- ifelse(status==0,"Empty Haul",
                 ifelse(status==1,"Full Haul",NA))
Maintitle <- ifelse(status==0,"Empty Truck Speed at OT",
               ifelse(status==1,"Loaded Truck Speed at OT",NA))

TruckStatus2 = "OPERATING"
yrange<-range(c(4761500,4765000)) #yrange<-range(c(4761000,4765100))
xrange<-range(c(648700,653500)) #xrange<-range(c(648500,657500))
pchid="."
colid1 = 2
colid2 = 3
colid3 = 4
colid4 = 5
colid5 = 6
colid6 = 7
colid7 = 8
colid8 = 9

#color code of speed 
x_speed_1<-ifelse(TruckSpeed$kph>=speedrange1 & TruckSpeed$kph<speedrange2 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_x,NA)
y_speed_1<-ifelse(TruckSpeed$kph>=speedrange1 & TruckSpeed$kph<speedrange2 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_y,NA)
x_speed_2<-ifelse(TruckSpeed$kph>=speedrange2 & TruckSpeed$kph<speedrange3 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_x,NA)
y_speed_2<-ifelse(TruckSpeed$kph>=speedrange2 & TruckSpeed$kph<speedrange3 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_y,NA)
x_speed_3<-ifelse(TruckSpeed$kph>=speedrange3 & TruckSpeed$kph<speedrange4 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_x,NA)
y_speed_3<-ifelse(TruckSpeed$kph>=speedrange3 & TruckSpeed$kph<speedrange4 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_y,NA)
x_speed_4<-ifelse(TruckSpeed$kph>=speedrange4 & TruckSpeed$kph<speedrange5 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_x,NA)
y_speed_4<-ifelse(TruckSpeed$kph>=speedrange4 & TruckSpeed$kph<speedrange5 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_y,NA)
x_speed_5<-ifelse(TruckSpeed$kph>=speedrange5 & TruckSpeed$kph<speedrange6 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_x,NA)
y_speed_5<-ifelse(TruckSpeed$kph>=speedrange5 & TruckSpeed$kph<speedrange6 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_y,NA)
x_speed_6<-ifelse(TruckSpeed$kph>=speedrange6 & TruckSpeed$kph<speedrange7 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_x,NA)
y_speed_6<-ifelse(TruckSpeed$kph>=speedrange6 & TruckSpeed$kph<speedrange7 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_y,NA)
x_speed_7<-ifelse(TruckSpeed$kph>=speedrange7 & TruckSpeed$kph<speedrange8 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_x,NA)
y_speed_7<-ifelse(TruckSpeed$kph>=speedrange7 & TruckSpeed$kph<speedrange8 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_y,NA)
x_speed_8<-ifelse(TruckSpeed$kph>speedrange8 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_x,NA)
y_speed_8<-ifelse(TruckSpeed$kph>speedrange8 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,TruckSpeed$position_y,NA)


#plotting 
#into one single chart
plot(x_speed_1,y_speed_1, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid1, pch=pchid, main=Maintitle)
par(new=TRUE)
plot(x_speed_2,y_speed_2, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid2, pch=pchid)
par(new=TRUE)
plot(x_speed_3,y_speed_3, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid3, pch=pchid)
par(new=TRUE)
plot(x_speed_4,y_speed_4, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid4, pch=pchid)
par(new=TRUE)
plot(x_speed_5,y_speed_5, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid5, pch=pchid)
par(new=TRUE)
plot(x_speed_6,y_speed_6, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid6, pch=pchid)
par(new=TRUE)
plot(x_speed_7,y_speed_7, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid7, pch=pchid)
par(new=TRUE)
plot(x_speed_8,y_speed_8, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid8, pch=pchid)
par(new=TRUE)
legend("bottomright",legend=c(speedlegend1,speedlegend2,speedlegend3,speedlegend4,speedlegend5,speedlegend6,speedlegend7,speedlegend8),col=c(colid1,colid2,colid3,colid4,colid5,colid6,colid7,colid8),pch="*",box.lty=0)

#STOP#

#Plot multiple graphs in different area
par(mfrow=c(2,4),oma=c(0,0,2,0))
plot(x_speed_1,y_speed_1, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid1, pch=pchid, main=speedlegend1)
plot(x_speed_2,y_speed_2, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid2, pch=pchid, main=speedlegend2)
plot(x_speed_3,y_speed_3, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid3, pch=pchid, main=speedlegend3)
plot(x_speed_4,y_speed_4, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid4, pch=pchid, main=speedlegend4)
plot(x_speed_5,y_speed_5, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid5, pch=pchid, main=speedlegend5)
plot(x_speed_6,y_speed_6, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid6, pch=pchid, main=speedlegend6)
plot(x_speed_7,y_speed_7, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid7, pch=pchid, main=speedlegend7)
plot(x_speed_8,y_speed_8, ylim=yrange,xlim=xrange, xlab="Easting", ylab="Northing",col=colid8, pch=pchid, main=speedlegend8)
title(Maintitle, outer=T)





# Different Section, this is for plotting using ggplot2 -------------------


# try out another type of plotting
# ggplot2 application
library(ggplot2)
# speed legend

TruckStatus1 = "Full Haul"
TruckStatus2 = "OPERATING"

TruckSpeed$speedgrp<-ifelse(TruckSpeed$kph>=0 & TruckSpeed$kph<10 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,0,
                        ifelse(TruckSpeed$kph>=10 & TruckSpeed$kph<15 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,10,
                          ifelse(TruckSpeed$kph>=15 & TruckSpeed$kph<20 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,15,
                            ifelse(TruckSpeed$kph>=20 & TruckSpeed$kph<25 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,20,
                              ifelse(TruckSpeed$kph>=25 & TruckSpeed$kph<30 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,25,
                                ifelse(TruckSpeed$kph>=30 & TruckSpeed$kph<40 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,30,
                                  ifelse(TruckSpeed$kph>=40 & TruckSpeed$kph<45 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,40,
                                    ifelse(TruckSpeed$kph>=45 & TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2,45,NA))))))))

new_x <-ifelse(TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2 & TruckSpeed$speedgrp>0,TruckSpeed$position_x,NA)
new_y <-ifelse(TruckSpeed$cycle_stage==TruckStatus1 & TruckSpeed$opreason==TruckStatus2 & TruckSpeed$speedgrp>0,TruckSpeed$position_y,NA)

p<- ggplot (TruckSpeed, aes(new_x, new_y))
p + geom_point(aes(colour=factor(TruckSpeed$speedgrp)))


