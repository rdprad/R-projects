blastholes <- read.csv(file.choose(), header = T) # Blastholes data from Dorjoo, C:\Rama\OC_Model_Clone\blastholes_data_complete.csv
phasename <- read.csv(file.choose(), header = T) # C:\Rama\OC_Model_Clone\Phase.csv
colnames(phasename)[1:3] <- c("xcentre", "ycentre", "zcentre")
resource10 <- read.csv(file.choose(), header = T) # C:\Rama\Resource\ots_2011_reblock_10x10x15.asc
resource10_trim <- resource10 %>% 
  select(xcentre, ycentre, zcentre, density, cu, au, class_smooth, as) %>% 
  mutate(elevation = zcentre-7.5) %>% 
  filter(elevation>=840 & elevation<=1140)
ocmodel <- read.csv(file.choose(), header = T) # C:\Rama\OC_Model_Clone\OCmodel.csv
colnames(ocmodel)[1:3] <- c("xcentre", "ycentre", "zcentre")
OC_options <- read.csv(file.choose(), header = T) # Nomin OC options, C:\Rama\OC_Model_Clone\OC_AU_options\OC_test_bench930_960_2018_within_not_phase_filter.asc
# listed below:
# option 3: sample selection min 1 and max 1, length above 10m
# option 4: sample selection min 1 and max 3, length above 10m
# option 5: sample selection min 1 and max 3, length above 10m, don't use the 930 pattern samples
# bau: current OC update, sample selection min 1 max 999
# au_strm: update STRM, sample selection min 4 and max 6

blastholes$code <- paste(blastholes$xcentre,blastholes$ycentre,blastholes$zcentre,sep="")
library(dplyr)
blastholes_unique <- blastholes %>% 
                        group_by(code) %>% 
                        slice(which.min(x3D_dist))

merge_blastholes <- merge(OC_options,blastholes_unique, by=c("xcentre", "ycentre", "zcentre"))
merge_blastholes <- merge(merge_blastholes,phasename, by=c("xcentre", "ycentre", "zcentre"))
merge_blastholes <- merge(merge_blastholes,resource10_trim, by=c("xcentre", "ycentre", "zcentre", "elevation"))
merge_blastholes$elevation <- factor(merge_blastholes$elevation, levels = rev(levels(merge_blastholes$elevation)))
levels(factor(merge_blastholes$elevation))
typeof(merge_blastholes$elevation)

merge_blastholes$cu_oc3_diff <- merge_blastholes$oc_cu3 - merge_blastholes$cu_blast
merge_blastholes$au_oc3_diff <- merge_blastholes$oc_au3 - merge_blastholes$au_blast
merge_blastholes$cu_oc4_diff <- merge_blastholes$oc_cu4 - merge_blastholes$cu_blast
merge_blastholes$au_oc4_diff <- merge_blastholes$oc_au4 - merge_blastholes$au_blast
merge_blastholes$cu_oc5_diff <- merge_blastholes$oc_cu5 - merge_blastholes$cu_blast
merge_blastholes$au_oc5_diff <- merge_blastholes$oc_au5 - merge_blastholes$au_blast

merge_blastholes$cu_oc3_diff_oc <- merge_blastholes$oc_cu3 - merge_blastholes$bcu
merge_blastholes$au_oc3_diff_oc <- merge_blastholes$oc_au3 - merge_blastholes$bau
merge_blastholes$cu_oc4_diff_oc <- merge_blastholes$oc_cu4 - merge_blastholes$bcu
merge_blastholes$au_oc4_diff_oc <- merge_blastholes$oc_au4 - merge_blastholes$bau
merge_blastholes$cu_oc5_diff_oc <- merge_blastholes$oc_cu5 - merge_blastholes$bcu
merge_blastholes$au_oc5_diff_oc <- merge_blastholes$oc_au5 - merge_blastholes$bau

merge_blastholes$cu_oc3_diff_strm <- merge_blastholes$oc_cu3 - merge_blastholes$cu_strm
merge_blastholes$au_oc3_diff_strm <- merge_blastholes$oc_au3 - merge_blastholes$au_strm
merge_blastholes$cu_oc4_diff_strm <- merge_blastholes$oc_cu4 - merge_blastholes$cu_strm
merge_blastholes$au_oc4_diff_strm <- merge_blastholes$oc_au4 - merge_blastholes$au_strm
merge_blastholes$cu_oc5_diff_strm <- merge_blastholes$oc_cu5 - merge_blastholes$cu_strm
merge_blastholes$au_oc5_diff_strm <- merge_blastholes$oc_au5 - merge_blastholes$au_strm

merge_blastholes$cu_oc3_diff_rsc <- merge_blastholes$oc_cu3 - merge_blastholes$cu
merge_blastholes$au_oc3_diff_rsc <- merge_blastholes$oc_au3 - merge_blastholes$au
merge_blastholes$cu_oc4_diff_rsc <- merge_blastholes$oc_cu4 - merge_blastholes$cu
merge_blastholes$au_oc4_diff_rsc <- merge_blastholes$oc_au4 - merge_blastholes$au
merge_blastholes$cu_oc5_diff_rsc <- merge_blastholes$oc_cu5 - merge_blastholes$cu
merge_blastholes$au_oc5_diff_rsc <- merge_blastholes$oc_au5 - merge_blastholes$au

library(ggplot2)
library(dplyr)
library(gridExtra) 

# Copper Plot compared to blastholes ============
P1 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc3_diff))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.3 vs. blastholes",
       subtitle = "Copper Head grade (OC opt3 - blastholes) : sample selection min 1 and max 1, length above 10m",
       fill="Cu(%) absolute difference")

P2 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc4_diff))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.4 vs. blastholes",
       subtitle = "Copper Head grade (OC opt4 - blastholes) : sample selection min 1 and max 3, length above 10m",
       fill="Cu(%) absolute difference")

P3 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc5_diff))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.5 vs. blastholes",
       subtitle = "Copper Head grade (OC opt5 - blastholes) : sample selection min 1 and max 3, length above 10m, don't use the 930 pattern samples",
       fill="Cu(%) absolute difference")

grid.arrange(P1, P2, P3, ncol=1)


# Copper Plot compared to OC model ============

P4 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc3_diff_oc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.3 vs. current OC model",
       subtitle = "Copper Head grade (OC opt3 - current OC model) : sample selection min 1 and max 1, length above 10m",
       fill="Cu(%) absolute difference")

P5 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc4_diff_oc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.4 vs. current OC model",
       subtitle = "Copper Head grade (OC opt4 - current OC model) : sample selection min 1 and max 3, length above 10m",
       fill="Cu(%) absolute difference")

P6 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc5_diff_oc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.5 vs. current OC model",
       subtitle = "Copper Head grade (OC opt5 - current OC model) : sample selection min 1 and max 3, length above 10m, don't use the 930 pattern samples",
       fill="Cu(%) absolute difference")

grid.arrange(P4, P5, P6, ncol=1)

# Gold Plot compared to blastholes ============
P7 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc3_diff))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.3 vs. blastholes",
       subtitle = "Gold Head grade (OC opt3 - blastholes) : sample selection min 1 and max 1, length above 10m",
       fill="Au(ppm) absolute difference")

P8 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc4_diff))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.4 vs. blastholes",
       subtitle = "Gold Head grade (OC opt4 - blastholes) : sample selection min 1 and max 3, length above 10m",
       fill="Au(ppm) absolute difference")

P9 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc5_diff))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.5 vs. blastholes",
       subtitle = "Gold Head grade (OC opt5 - blastholes) : sample selection min 1 and max 3, length above 10m, don't use the 930 pattern samples",
       fill="Au(ppm) absolute difference")

grid.arrange(P7, P8, P9, ncol=1)


# Gold Plot compared to OC model ============

P10 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc3_diff_oc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.3 vs. current OC model",
       subtitle = "Gold Head grade (OC opt3 - current OC model) : sample selection min 1 and max 1, length above 10m",
       fill="Au(ppm) absolute difference")

P11 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc4_diff_oc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.4 vs. current OC model",
       subtitle = "Gold Head grade (OC opt4 - current OC model) : sample selection min 1 and max 3, length above 10m",
       fill="Au(ppm) absolute difference")

P12 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc5_diff_oc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.5 vs. current OC model",
       subtitle = "Gold Head grade (OC opt5 - current OC model) : sample selection min 1 and max 3, length above 10m, don't use the 930 pattern samples",
       fill="Au(ppm) absolute difference")

grid.arrange(P10, P11, P12, ncol=1)

# Copper Plot compared to STRM model ============

P13 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc3_diff_strm))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.3 vs. STRM model",
       subtitle = "Copper Head grade (OC opt3 - STRM model) : sample selection min 1 and max 1, length above 10m",
       fill="Cu(%) absolute difference")

P14 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc4_diff_strm))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.4 vs. STRM model",
       subtitle = "Copper Head grade (OC opt4 - STRM model) : sample selection min 1 and max 3, length above 10m",
       fill="Cu(%) absolute difference")

P15 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc5_diff_strm))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.5 vs. STRM model",
       subtitle = "Copper Head grade (OC opt5 - STRM model) : sample selection min 1 and max 3, length above 10m, don't use the 930 pattern samples",
       fill="Cu(%) absolute difference")

grid.arrange(P13, P14, P15, ncol=1)

# Copper Plot compared to RESOURCE model ============

P16 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc3_diff_rsc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.3 vs. Resource model",
       subtitle = "Copper Head grade (OC opt3 - Resource model) : sample selection min 1 and max 1, length above 10m",
       fill="Cu(%) absolute difference")

P17 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc4_diff_rsc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.4 vs. Resource model",
       subtitle = "Copper Head grade (OC opt4 - Resource model) : sample selection min 1 and max 3, length above 10m",
       fill="Cu(%) absolute difference")

P18 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=cu_oc5_diff_rsc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.5 vs. Resource model",
       subtitle = "Copper Head grade (OC opt5 - Resource model) : sample selection min 1 and max 3, length above 10m, don't use the 930 pattern samples",
       fill="Cu(%) absolute difference")

grid.arrange(P16, P17, P18, ncol=1)

# Gold Plot compared to STRM model ============

P19 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc3_diff_strm))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.3 vs. STRM model",
       subtitle = "Gold Head grade (OC opt3 - STRM model) : sample selection min 1 and max 1, length above 10m",
       fill="Au(ppm) absolute difference")

P20 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc4_diff_strm))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.4 vs. STRM model",
       subtitle = "Gold Head grade (OC opt4 - STRM model) : sample selection min 1 and max 3, length above 10m",
       fill="Au(ppm) absolute difference")

P21 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc5_diff_strm))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.5 vs. STRM model",
       subtitle = "Gold Head grade (OC opt5 - STRM model) : sample selection min 1 and max 3, length above 10m, don't use the 930 pattern samples",
       fill="Au(ppm) absolute difference")

grid.arrange(P19, P20, P21, ncol=1)

# Gold Plot compared to RESOURCE model ============

P22 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc3_diff_rsc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.3 vs. Resource model",
       subtitle = "Gold Head grade (OC opt3 - Resource model) : sample selection min 1 and max 1, length above 10m",
       fill="Au(ppm) absolute difference")

P23 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc4_diff_rsc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.4 vs. Resource model",
       subtitle = "Gold Head grade (OC opt4 - Resource model) : sample selection min 1 and max 3, length above 10m",
       fill="Au(ppm) absolute difference")

P24 <- merge_blastholes %>% 
  filter(elevation %in% c(960:930), PHASE==4) %>% 
  ggplot(aes(x=xcentre, y=ycentre))+
  geom_raster(aes(fill=au_oc5_diff_rsc))+
  scale_fill_gradientn(colours = c("darkred", "yellow", "grey", "cyan", "purple"),
                       values = scales::rescale(c(-1.0, -0.5, -0.1, 0, 0.1, 0.5, 1.0)),
                       guide = "colorbar", limits=c(-1.0,1.0))+
  coord_fixed(ratio=1)+
  facet_wrap(~elevation, ncol = 4)+
  theme_bw()+
  theme(legend.position="right",
        panel.grid = element_blank())+
  labs(title = "Phase 4a spatial reconciliation OC model opt.5 vs. Resource model",
       subtitle = "Gold Head grade (OC opt5 - Resource model) : sample selection min 1 and max 3, length above 10m, don't use the 930 pattern samples",
       fill="Au(ppm) absolute difference")

grid.arrange(P22, P23, P24, ncol=1)
