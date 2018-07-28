ggplot(data.frame(x=c(-10,10)), aes(x=x))+
  stat_function(fun = function(x)(x+5))

model = lm(merge_blastholes$cu~merge_blastholes$au)                
summary(model)

library(PerformanceAnalytics)
chart.Correlation(merge_blastholes[,grep("cu_",colnames(merge_blastholes))], histogram = T, pch=19)

names(merge_blastholes)=="cu"
grep("^cu_", colnames(merge_blastholes))

colnames(merge_blastholes[,grep("cu",colnames(merge_blastholes))])


deleteme <- read.csv(file.choose(), header=T)
View(deleteme)

deleteme$x1 <- ifelse(round(deleteme$botx/10,0)*10-deleteme$botx>0,round(deleteme$botx/10,0)*10-5,
                      round(deleteme$botx/10,0)*10+5)
deleteme$y1 <- ifelse(round(deleteme$boty/10,0)*10-deleteme$boty>0,round(deleteme$boty/10,0)*10-5,
                      round(deleteme$boty/10,0)*10+5)
deleteme$z1 <- ifelse(round(deleteme$botz/15,0)*15-deleteme$botz>0,round(deleteme$botz/15,0)*15-7.5,
                      round(deleteme$botz/15,0)*15+7.5)
deleteme$z1 <- deleteme$z1 + 7.5

summary(deleteme$botx-deleteme$x1)
summary(deleteme$boty-deleteme$y1)
summary(deleteme$botz-deleteme$z1)
summary(deleteme$xcentre-deleteme$x1)
summary(deleteme$ycentre-deleteme$y1)
summary(deleteme$elevation-deleteme$z1)
sum((deleteme$elevation-deleteme$z1)==0)

deleteme$x1 <- deleteme$botx%/%10*10+5
deleteme$y1 <- deleteme$boty%/%10*10+5
deleteme$z1 <- round(deleteme$botz/15,0)*15


table_au$OC_tonnes*table_au$OC_ave_au
as.data.frame(table(table_au$elevation))                     


merge_model %>% 
  filter(elevation<=930, PHASE==4, cu>0.37) %>% 
  ggplot()+
    geom_density(aes(f, col=rank))+
    scale_colour_manual(values=c("orange","red"))+
    facet_wrap(~elevation, nrow=2)+
    theme_bw()
  