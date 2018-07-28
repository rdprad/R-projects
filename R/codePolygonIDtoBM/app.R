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


# BMframe$elevation <- BMframe$zcentre - 7.5
library(sp)
library(sf)
library(rgdal)
BMframe_sp <- BMframe
coordinates(BMframe_sp) <- c("xcentre", "ycentre")

#import polygon from minesight
poly_shp <- readOGR(dsn="C:/Rama/OC_Model_Clone/all-polygon.shp")

BMframe_sp$ID <- sapply(seq(900,930, by=15), function(x){
 cbind(paste(over(BMframe_sp, subset(poly_shp, BENCH==x))$cutName,
                     over(BMframe_sp, subset(poly_shp, BENCH==x))$BENCH, sep=","))
})

BMframe_df <- as.data.frame(BMframe_sp)

tmp =list()
result = list()
for (i in 1:(length(BMframe_df)-2)) {
 tmp <- cbind(BMframe_df[,c(grep("xcentre",colnames(BMframe_df)),grep("ycentre",colnames(BMframe_df)))],ID=BMframe_df[,i])
 result[[i]] <- tmp
}

big.data = do.call(rbind,result)
big.data <- big.data %>% 
 filter(!grepl("NA", ID))

library(dplyr)
library(ggplot2)
big.data %>% 
 filter(!grepl("NA", ID), grepl("900", ID)) %>% 
 ggplot(aes(x=xcentre, y=ycentre))+
 geom_raster(aes(fill=ID))+
 guides(fill=FALSE)+
 coord_equal(ratio=1)

library(raster)
(subset(poly_shp, grepl("1005_406_411_LG", cutName)))*15
