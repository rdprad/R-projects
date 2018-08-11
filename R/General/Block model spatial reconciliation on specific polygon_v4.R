# Recreate block model frame grids (3D)
BMframe <- list(xcentre=seq(649205, by=10, length.out = 330),
                ycentre=seq(4761605, by=10, length.out = 380),
                zcentre=seq(-22.5, by=15, length.out = 82))
BMframe1 <- merge(BMframe$xcentre, BMframe$zcentre)
colnames(BMframe1)[1:2] <- c("xcentre", "zcentre")
BMframe2 <- merge(BMframe$ycentre, BMframe$zcentre)
colnames(BMframe2)[1:2] <- c("ycentre", "zcentre")
BMframe <- merge(BMframe1, BMframe2, by="zcentre")
BMframe <- BMframe[,c(2,3,1)]
rm(BMframe1, BMframe2)

# Recreate block model frame grids (2D)
BMframe <- list(xcentre=seq(649205, by=10, length.out = 330),
                ycentre=seq(4761605, by=10, length.out = 380))
BMframe <- merge(BMframe$xcentre, BMframe$ycentre)
colnames(BMframe)[1:2] <- c("xcentre", "ycentre")

#import polygon from minesight
library(sp)
library(sf)
library(rgdal)
poly_shp <- readOGR(dsn="")

# import block models
resource10 <- read.csv("", header = T)
resource10_trim <- resource10 %>%
                      filter(xcentre>64933,xcentre<651820,
                      ycentre>4762030, ycentre<4764930,
                      zcentre>800, zcentre<=1170) %>% 
                      select(xcentre, ycentre, zcentre, cu, au, as, density)
                      
library(odbc)
OCcon <- dbConnect(odbc::odbc(), "")
OCcon_df <- dbGetQuery(OCcon, "select xworld, yworld, zworld, BCU, BAU, BAS, SG FROM OTVulcan.dbo.OTBM WHERE zworld>800")
colnames(OCcon_df)[1:3] <- c("xcentre", "ycentre", "zcentre")
OCcon_df <- OCcon_df %>% 
  filter(xcentre>64933,xcentre<651820,
         ycentre>4762030, ycentre<4764930,
         zcentre>800, zcentre<=1170)

# phasename <- read.csv("", header = T)
# phasename <- phasename %>% 
#                 filter(PHASE>0)
colnames(phasename)[1:3] <- c("xcentre", "ycentre", "zcentre")
merge_model <- merge(resource10_trim, OCcon_df, by=c("xcentre", "ycentre", "zcentre"))
# merge_model <- merge(merge_model, phasename, by=c("xcentre", "ycentre", "zcentre"))
merge_model <- merge_model %>% 
                filter(xcentre>64933,xcentre<651820,
                       ycentre>4762030, ycentre<4764930,
                       zcentre>800, zcentre<=1170)

#Variables definition, STARTS FROM HERE!!!
elev = 915
phaseid = paste(c(1,2,3,4,5,6,7,8,9,10,42,62),collapse = "|")
metal <- "gold" # select between gold, copper, arsenic

metal1 <- ifelse(metal=="gold", "BAU",
                 ifelse(metal=="copper", "BCU",
                        ifelse(metal=="arsenic", "BAS",
                        NA))) # defining standard code for ore control grade item name
metal2 <- ifelse(metal=="gold", "au",
                 ifelse(metal=="copper", "cu",
                        ifelse(metal=="arsenic", "as",
                        NA))) # defining standard code for resource grade item name

# test work on 2D 
Phase_plot = c(4)
BMframe_df <- BMframe
coordinates(BMframe_df) <- c("xcentre", "ycentre")
poly_shp2 <- subset(poly_shp,poly_shp$BENCH==elev)
poly_shp2 <- subset(poly_shp2,grepl(Phase_plot,poly_shp2$Phase))
BMframe_df$cutName <- over(BMframe_df,poly_shp2)$cutName
BMframe_df$Phase <- over(BMframe_df,poly_shp2)$Phase
BMframe_df <- as.data.frame(BMframe_df)
BMframe_df$zcentre <- elev+7.5

# merging data and recalculating items
merge_model_plot <- merge(merge_model, BMframe_df, by=c("xcentre", "ycentre", "zcentre"))
merge_model_plot$diff <- merge_model_plot[,grep(paste("^",metal1,"$",sep=""),colnames(merge_model_plot))]-
  merge_model_plot[,grep(paste("^",metal2,"$",sep=""),colnames(merge_model_plot))]
  
#plotting based on 2D
library(dplyr)
library(ggplot2)

polygon_name = c("MG","HG","CRS") # add as necessary, to plot all blocks just type: elev
poly_shp3 <- subset(poly_shp2, grepl(paste(polygon_name,collapse = "|"),cutName))
max_diff <- ifelse(metal == "arsenic", 750,1)
min_diff <- ifelse(metal == "arsenic", -750,-1)
map_plot <- merge_model_plot %>% 
  filter(!is.na(cutName), grepl(paste(polygon_name,collapse="|"),cutName), Phase %in% Phase_plot) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=diff))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(min_diff, 0.5*min_diff, 0.1*min_diff, 0, 0.1*max_diff, 0.5*max_diff, max_diff)),
                       guide = "colorbar", limits=c(min_diff,max_diff))+
  geom_polygon(data = poly_shp2, aes(long, lat, group=group), col="grey",fill=NA, alpha=0.5)+
  geom_polygon(data = poly_shp3, aes(long, lat, group=group), col="blue", fill=NA, size=1)+ 
  coord_equal(ratio=1)+
  theme_bw()+
  labs(title = paste(metal,"absolute differences in phase",paste(Phase_plot, collapse = " & "),"elevation",elev),
       subtitle = paste("Filtered by polygon:",paste(polygon_name, collapse = " & ")),
       fill = paste(metal1,"-",metal2),
       x="EASTING",
       y="NORTHING")

# End of plot==========

# grade tonnage curve=========
# binning

merge_model_plot$OC_bin <- ifelse(metal == "gold" & merge_model_plot$BAU>=1.0,1.0,
                             ifelse(metal == "gold" & merge_model_plot$BAU>=0.9,0.9,
                               ifelse(metal == "gold" & merge_model_plot$BAU>=0.8,0.8,
                                 ifelse(metal == "gold" & merge_model_plot$BAU>=0.7,0.7,
                                   ifelse(metal == "gold" & merge_model_plot$BAU>=0.6,0.6,
                                     ifelse(metal == "gold" & merge_model_plot$BAU>=0.5,0.5,
                                       ifelse(metal == "gold" & merge_model_plot$BAU>=0.4,0.4,
                                         ifelse(metal == "gold" & merge_model_plot$BAU>=0.3,0.3,
                                           ifelse(metal == "gold" & merge_model_plot$BAU>=0.2,0.2,
                                             ifelse(metal == "gold" & merge_model_plot$BAU>=0.1,0.1,
                           ifelse(metal == "copper" & merge_model_plot$BCU>=1.0,1.0,
                             ifelse(metal == "copper" & merge_model_plot$BCU>=0.9,0.9,
                               ifelse(metal == "copper" & merge_model_plot$BCU>=0.8,0.8,
                                 ifelse(metal == "copper" & merge_model_plot$BCU>=0.7,0.7,
                                   ifelse(metal == "copper" & merge_model_plot$BCU>=0.6,0.6,
                                     ifelse(metal == "copper" & merge_model_plot$BCU>=0.5,0.5,
                                       ifelse(metal == "copper" & merge_model_plot$BCU>=0.4,0.4,
                                         ifelse(metal == "copper" & merge_model_plot$BCU>=0.3,0.3,
                                           ifelse(metal == "copper" & merge_model_plot$BCU>=0.2,0.2,
                                             ifelse(metal == "copper" & merge_model_plot$BCU>=0.1,0.1,
                           ifelse(metal == "arsenic" & merge_model_plot$BAS>=500,500,
                             ifelse(metal == "arsenic" & merge_model_plot$BAS>=450,450,
                               ifelse(metal == "arsenic" & merge_model_plot$BAS>=400,400,
                                 ifelse(metal == "arsenic" & merge_model_plot$BAS>=350,350,
                                   ifelse(metal == "arsenic" & merge_model_plot$BAS>=300,300,
                                     ifelse(metal == "arsenic" & merge_model_plot$BAS>=250,250,
                                       ifelse(metal == "arsenic" & merge_model_plot$BAS>=200,200,
                                         ifelse(metal == "arsenic" & merge_model_plot$BAS>=150,150,
                                           ifelse(metal == "arsenic" & merge_model_plot$BAS>=100,100,
                                             ifelse(metal == "arsenic" & merge_model_plot$BAS>=50,50,
                                                ifelse(metal == "arsenic" & merge_model_plot$as>=45,45,
                                                 ifelse(metal == "arsenic" & merge_model_plot$as>=40,40,
                                                   ifelse(metal == "arsenic" & merge_model_plot$as>=35,35,
                                                     ifelse(metal == "arsenic" & merge_model_plot$as>=30,30,
                                                       ifelse(metal == "arsenic" & merge_model_plot$as>=25,25,
                                                         ifelse(metal == "arsenic" & merge_model_plot$as>=20,20,
                                                           ifelse(metal == "arsenic" & merge_model_plot$as>=15,15,
                                                             ifelse(metal == "arsenic" & merge_model_plot$as>=10,10,
                                                               ifelse(metal == "arsenic" & merge_model_plot$as>=5,5,0
                                             )))))))))))))))))))))))))))))))))))))))
merge_model_plot$RSC_bin <- ifelse(metal == "gold" & merge_model_plot$au>=1.0,1.0,
                             ifelse(metal == "gold" & merge_model_plot$au>=0.9,0.9,
                               ifelse(metal == "gold" & merge_model_plot$au>=0.8,0.8,
                                 ifelse(metal == "gold" & merge_model_plot$au>=0.7,0.7,
                                   ifelse(metal == "gold" & merge_model_plot$au>=0.6,0.6,
                                     ifelse(metal == "gold" & merge_model_plot$au>=0.5,0.5,
                                       ifelse(metal == "gold" & merge_model_plot$au>=0.4,0.4,
                                         ifelse(metal == "gold" & merge_model_plot$au>=0.3,0.3,
                                           ifelse(metal == "gold" & merge_model_plot$au>=0.2,0.2,
                                             ifelse(metal == "gold" & merge_model_plot$au>=0.1,0.1,
                           ifelse(metal == "copper" & merge_model_plot$cu>=1.0,1.0,
                             ifelse(metal == "copper" & merge_model_plot$cu>=0.9,0.9,
                               ifelse(metal == "copper" & merge_model_plot$cu>=0.8,0.8,
                                 ifelse(metal == "copper" & merge_model_plot$cu>=0.7,0.7,
                                   ifelse(metal == "copper" & merge_model_plot$cu>=0.6,0.6,
                                     ifelse(metal == "copper" & merge_model_plot$cu>=0.5,0.5,
                                       ifelse(metal == "copper" & merge_model_plot$cu>=0.4,0.4,
                                         ifelse(metal == "copper" & merge_model_plot$cu>=0.3,0.3,
                                           ifelse(metal == "copper" & merge_model_plot$cu>=0.2,0.2,
                                             ifelse(metal == "copper" & merge_model_plot$cu>=0.1,0.1,
                           ifelse(metal == "arsenic" & merge_model_plot$as>=500,500,
                             ifelse(metal == "arsenic" & merge_model_plot$as>=450,450,
                               ifelse(metal == "arsenic" & merge_model_plot$as>=400,400,
                                 ifelse(metal == "arsenic" & merge_model_plot$as>=350,350,
                                   ifelse(metal == "arsenic" & merge_model_plot$as>=300,300,
                                     ifelse(metal == "arsenic" & merge_model_plot$as>=250,250,
                                       ifelse(metal == "arsenic" & merge_model_plot$as>=200,200,
                                         ifelse(metal == "arsenic" & merge_model_plot$as>=150,150,
                                           ifelse(metal == "arsenic" & merge_model_plot$as>=100,100,
                                             ifelse(metal == "arsenic" & merge_model_plot$as>=50,50,
                                               ifelse(metal == "arsenic" & merge_model_plot$as>=45,45,
                                                 ifelse(metal == "arsenic" & merge_model_plot$as>=40,40,
                                                   ifelse(metal == "arsenic" & merge_model_plot$as>=35,35,
                                                     ifelse(metal == "arsenic" & merge_model_plot$as>=30,30,
                                                       ifelse(metal == "arsenic" & merge_model_plot$as>=25,25,
                                                         ifelse(metal == "arsenic" & merge_model_plot$as>=20,20,
                                                           ifelse(metal == "arsenic" & merge_model_plot$as>=15,15,
                                                             ifelse(metal == "arsenic" & merge_model_plot$as>=10,10,
                                                               ifelse(metal == "arsenic" & merge_model_plot$as>=5,5,0
                                             )))))))))))))))))))))))))))))))))))))))

# write up table for grade tonnage curve
gt_table <- merge(
merge_model_plot %>% 
  filter(!is.na(cutName), grepl(paste(polygon_name,collapse="|"),cutName), Phase %in% Phase_plot) %>% 
  group_by(OC_bin) %>% 
  mutate(massOC = 10*10*15*SG) %>% 
  summarise(OC_tonnes = sum(massOC),
            OC_metal = ifelse(metal=="gold",sum(BAU*massOC),
                              ifelse(metal=="copper",sum(BCU*massOC),
                                     ifelse(metal=="arsenic",sum(BAS*massOC))))) %>% 
  arrange(desc(OC_bin)) %>% 
  mutate(OC_cumtonnes = cumsum(OC_tonnes),
         OC_ave_grade = cumsum(OC_metal)/OC_cumtonnes) %>% 
  arrange(OC_bin) %>% 
  rename(grade_bin=OC_bin),
merge_model_plot %>% 
  filter(!is.na(cutName), grepl(paste(polygon_name,collapse="|"),cutName), Phase %in% Phase_plot) %>% 
  group_by(RSC_bin) %>% 
  mutate(massRSC = 10*10*15*density) %>% 
  summarise(RSC_tonnes = sum(massRSC),
            RSC_metal = ifelse(metal=="gold",sum(au*massRSC),
                              ifelse(metal=="copper",sum(cu*massRSC),
                                     ifelse(metal=="arsenic",sum(as*massRSC))))) %>% 
  arrange(desc(RSC_bin)) %>% 
  mutate(RSC_cumtonnes = cumsum(RSC_tonnes),
         RSC_ave_grade = cumsum(RSC_metal)/RSC_cumtonnes) %>% 
  arrange(RSC_bin) %>% 
  rename(grade_bin=RSC_bin),
by=c("grade_bin"))


#Plot grade tonnage curve
library(ggplot2)
  gt_plot <-   ggplot(data= gt_table, aes(x=grade_bin))+
  geom_line(aes(y=OC_cumtonnes, col="OC tonnes (line)"), linetype = "solid")+
  geom_line(aes(y=RSC_cumtonnes, col="Resource tonnes (dashed)"), linetype = "longdash")+
  geom_line(aes(y=OC_ave_grade*max(OC_cumtonnes)/max(OC_ave_grade), col="OC grades (line)"), linetype = "solid")+
  geom_line(aes(y=RSC_ave_grade*max(OC_cumtonnes)/max(OC_ave_grade), col="Resource grades (dashed)"), linetype = "longdash")+
  scale_color_manual(values=c("red","blue","red","blue"))+
  scale_y_continuous(labels = scales::comma, breaks = seq(0,max(gt_table$OC_cumtonnes)+500000,by=500000), 
                     sec.axis = sec_axis(~./((max(gt_table$OC_cumtonnes)/max(gt_table$OC_ave_grade))), name = "Average Grade",
                                         breaks = seq(0,(max(gt_table$OC_cumtonnes)+500000)/(max(gt_table$OC_cumtonnes)/max(gt_table$OC_ave_grade)),by= ifelse(metal=="arsenic",round(max(gt_table$OC_ave_grade)/100,0)*10,0.1))))+
  labs(x="cut-off head grade",
       y="Tonnage",
       col="LEGENDS")+
  coord_fixed(ratio= ifelse(metal=="arsenic",max(gt_table$OC_ave_grade),1)/max(gt_table$OC_cumtonnes))+
  theme_bw()+
  theme(legend.position = "right")

library(gridExtra)  
grid.arrange(map_plot,gt_plot,nrow=1) 

