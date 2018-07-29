 library(rCharts)
  library(doBy)
  library(reshape2)
  set.seed(1)
  df <- data.frame(date=as.Date("2014-01-01")+1:10,y1=sample(1:200,70),y2=runif(70))

  buildchart <- function(df) {
  plotData<-df
  temp<-aggregate(cbind(y1)~date,data=df,min)
  tabledta<-summaryBy(y2~date,data=df,FUN=sum,keep.names = TRUE) 
  tabledta<-merge(tabledta,temp,by=c("date"),all.x=T)

  filler = expand.grid(date=seq(as.Date(min(tabledta$date)),max(as.Date(tabledta$date)),by='1 day'))
  df = merge(filler,
             tabledta,
             by=c('date'),
             all.x=T)
  df[is.na(df)]=0
  data.melt<-melt(df,id.vars=c('date'),all=TRUE)


  #Changed to 'multichart'
  p <- nPlot(value ~ date, group = 'variable', data = data.melt, type = 'multiChart')
  p$params$multi = list(
  y1 = list(type="line",yAxis=1),  y2 = list(type="line",yAxis=2))


  #The next two are commented two avoid incompatibilities
  #p$chart(margin=list(left=150))
  #p$yAxis(showMaxMin = FALSE)

  p$xAxis(tickFormat ="#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d * 24 * 60 * 60 * 1000));}!#")

  p$setTemplate(script = system.file(
  "/libraries/nvd3/layouts/multiChart.html",
  package = "rCharts"))
  p
  }

  buildchart(df) 