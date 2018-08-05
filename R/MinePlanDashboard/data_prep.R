# New data using csv files-----------
library(dplyr)
library(lubridate)
library(zoo)
library(rCharts)
Plan_8Q3 <- read.csv(file="C:/Rama/8Q3_fullreport.csv", header = T)
colnames(Plan_8Q3) <- make.unique(colnames(Plan_8Q3), sep = ".")

tmp <- tibble(Period = c(2:40), 
            PeriodID = c(as.character(as.yearmon(seq(as.Date("2018-10-1"), as.Date("2020-12-1"), by = "month"))),
                         "21Q1", "21Q2", "21Q3", "21Q4",
                         "22Q1", "22Q2", "22Q3", "22Q4",
                         "23Q1", "23Q2", "23Q3", "23Q4"),
            Year = c(rep(2018,3), rep(2019,12), rep(2020,12),
                     rep(2021,4), rep(2022,4), rep(2023,4)),
            Month = c(c(10:12), rep(c(1:12),2), rep(c(3,6,9,12),3)),
            MonthID = as.Date(paste(Year, Month,1, sep = "-"), "%Y-%m-%d"))

# tmp <- tibble(Period = c(1:40), 
#             PeriodID = c("start", as.character(as.yearmon(seq(as.Date("2018-10-1"), as.Date("2020-12-1"), by = "month"))),
#                          "21Q1", "21Q2", "21Q3", "21Q4",
#                          "22Q1", "22Q2", "22Q3", "22Q4",
#                          "23Q1", "23Q2", "23Q3", "23Q4"),
#             Year = c(1900, rep(2018,3), rep(2019,12), rep(2020,12),
#                      rep(2021,4), rep(2022,4), rep(2023,4)),
#             Month = c(1,c(10:12), rep(c(1:12),2), rep(c(3,6,9,12),3)),
#             MonthID = as.Date(paste(Year, Month,1, sep = "-"), "%Y-%m-%d"))

# tmp_b <- merge(tmp, data.frame(Area = c("Rehandle", "UG", "Phs04a", "Phs04b", "Phs04b-n", "Phs05", "Phs06", "Phs06b", "Phs07")) )

tmp$PeriodID <- as.factor(tmp$PeriodID)
# fn = factor(f, levels=unique(f[order(a,b,f)]), ordered=TRUE)
tmp$PeriodID <- factor(tmp$PeriodID, levels=unique(tmp$PeriodID[order(tmp$Period)]), ordered=TRUE) 
# tmp$MonthID <- factor(tmp$MonthID, levels=unique(tmp$MonthID[order(tmp$Period)]), ordered=TRUE) 
levels(factor(tmp$PeriodID))
# levels(tmp$MonthID)
Plan_8Q3_a <- merge(Plan_8Q3, tmp, by = c("Period"))

a <- merge(Plan_8Q3_a %>% 
 group_by(Phase, PeriodID) %>% 
 mutate(Area = ifelse((grepl("Phs", Phase) | grepl("UG", Phase)) & (Source == "EX_PIT" | Source == "UG"), paste(Phase), "Rehandle")) %>% 
 group_by(Year, PeriodID, Area) %>% 
 summarise(TMM = round(sum(Tonnage),0)),
 Plan_8Q3_a %>% 
 group_by(Phase, PeriodID) %>% 
 filter(Destination %in% "CRUSHER_DST") %>% 
 mutate(Area = ifelse((grepl("Phs", Phase) | grepl("UG", Phase)) & (Source == "EX_PIT" | Source == "UG"), paste(Phase), "Rehandle")) %>%
 group_by(Year, PeriodID, Area) %>% 
 summarise(Crusher_t = round(sum(Tonnage),0),
           millhrs = sum(MHRS),
           Cu_pct = weighted.mean(CU, Tonnage),
           Cu_rec = round(weighted.mean(RECCU, Tonnage)/Cu_pct*100,2),
           Cu_kt = sum(Tonnage)*Cu_pct/100*Cu_rec/100/1000,
           Au_ppm = weighted.mean(AU, Tonnage),
           Au_rec =round(weighted.mean(RECAU, Tonnage)/Au_ppm*100,2),
           Au_koz = sum(Tonnage)*Au_ppm*Au_rec/100/31.10348/1000)
 , by =c("Year", "PeriodID", "Area"), all = T)
a$Area <- factor(a$Area, levels = c("Rehandle", "UG", "Phs04a", "Phs04b", "Phs04b-n", "Phs05", "Phs06", "Phs06b", "Phs07"))
tmp_b <- a[,c(1:2)]
tmp_b <- tmp_b %>% 
 filter(!duplicated(PeriodID))
tmp_b <- merge(tmp_b, data.frame(Area = c("Rehandle", "UG", "Phs04a", "Phs04b", "Phs04b-n", "Phs05", "Phs06", "Phs06b", "Phs07")), all=T )
a <- merge(a, tmp_b, by =c("Year", "PeriodID", "Area"), all = T)
a <- a %>% 
 replace(.,is.na(.),0)
a <- a[-grep("start", a$PeriodID),]

b <- merge(Plan_8Q3_a %>% 
 group_by(Phase, Year) %>% 
 mutate(Area = ifelse((grepl("Phs", Phase) | grepl("UG", Phase)) & (Source == "EX_PIT" | Source == "UG"), paste(Phase), "Rehandle")) %>% 
 group_by(Year, Area) %>% 
 summarise(TMM = round(sum(Tonnage),0)),
 Plan_8Q3_a %>% 
 group_by(Phase, Year) %>% 
 filter(Destination %in% "CRUSHER_DST") %>% 
 mutate(Area = ifelse((grepl("Phs", Phase) | grepl("UG", Phase)) & (Source == "EX_PIT" | Source == "UG"), paste(Phase), "Rehandle")) %>% 
 group_by(Year, Area) %>% 
 summarise(Crusher_t = round(sum(Tonnage),0),
           millhrs = sum(MHRS),
           Cu_pct = weighted.mean(CU, Tonnage),
           Cu_rec = round(weighted.mean(RECCU, Tonnage)/Cu_pct*100,2),
           Cu_kt = sum(Tonnage)*Cu_pct/100*Cu_rec/100/1000,
           Au_ppm = weighted.mean(AU, Tonnage),
           Au_rec =round(weighted.mean(RECAU, Tonnage)/Au_ppm*100,2),
           Au_koz = sum(Tonnage)*Au_ppm*Au_rec/100/31.10348/1000),
 by=c("Year", "Area"))

b$Area <- factor(b$Area, levels = c("Rehandle", "UG", "Phs04a", "Phs04b", "Phs04b-n", "Phs05", "Phs06", "Phs06b", "Phs07"))

c <- rbind(Plan_8Q3_a %>% 
 group_by(Phase, PeriodID) %>% 
 filter(Destination %in% "CRUSHER_DST") %>% 
 mutate(Area = ifelse((grepl("Phs", Phase) | grepl("UG", Phase)) & (Source == "EX_PIT" | Source == "UG"), paste(Phase), "Rehandle")) %>% 
 group_by(Year, PeriodID) %>% 
 summarise(Crusher_t = round(sum(Tonnage),0),
           millhrs = sum(MHRS),
           grade = round(weighted.mean(CU, Tonnage),3),
           recovery = round(weighted.mean(RECCU, Tonnage)/grade*100,2),
           metal = round(sum(Tonnage)*grade/100*recovery/100/1000,1),
           group = "Copper"),
Plan_8Q3_a %>% 
 group_by(Phase, PeriodID) %>% 
 filter(Destination %in% "CRUSHER_DST") %>% 
 mutate(Area = ifelse((grepl("Phs", Phase) | grepl("UG", Phase)) & (Source == "EX_PIT" | Source == "UG"), paste(Phase), "Rehandle")) %>% 
 group_by(Year, PeriodID) %>% 
 summarise(Crusher_t = round(sum(Tonnage),0),
           millhrs = sum(MHRS),
           grade = round(weighted.mean(AU, Tonnage),3),
           recovery = round(weighted.mean(RECAU, Tonnage)/grade*100,2),
           metal = round(sum(Tonnage)*grade*recovery/100/31.10348/1000,1),
           group = "Gold"))

# c1 <- Plan_8Q3_a %>% 
#  group_by(Phase, PeriodID) %>% 
#  filter(Destination %in% "CRUSHER_DST") %>% 
#  mutate(Area = ifelse((grepl("Phs", Phase) | grepl("UG", Phase)) & (Source == "EX_PIT" | Source == "UG"), paste(Phase), "Rehandle")) %>% 
#  group_by(Year, PeriodID) %>% 
#  summarise(Crusher_t = round(sum(Tonnage),0),
#            millhrs = sum(MHRS),
#            grade = round(weighted.mean(CU, Tonnage),3),
#            recovery = round(weighted.mean(RECCU, Tonnage)/grade*100,2),
#            metal = round(sum(Tonnage)*grade/100*recovery/100/1000,1),
#            group = "Copper")
# 
# c2 <- Plan_8Q3_a %>% 
#  group_by(Phase, PeriodID) %>% 
#  filter(Destination %in% "CRUSHER_DST") %>% 
#  mutate(Area = ifelse((grepl("Phs", Phase) | grepl("UG", Phase)) & (Source == "EX_PIT" | Source == "UG"), paste(Phase), "Rehandle")) %>% 
#  group_by(Year, PeriodID) %>% 
#  summarise(Crusher_t = round(sum(Tonnage),0),
#            millhrs = sum(MHRS),
#            grade = round(weighted.mean(AU, Tonnage),3),
#            recovery = round(weighted.mean(RECAU, Tonnage)/grade*100,2),
#            metal = round(sum(Tonnage)*grade*recovery/100/31.10348/1000,1),
#            group = "Gold")

d <- read.csv(file="C:/Rama/R/github/rdprad/R-projects/R/MinePlan_Dashboard/valuebox_mineplan.csv", header = T)
d <- merge(d, tmp, by=c("Period"))

d1 <- d %>% 
 group_by(Year, PeriodID) %>% 
 summarise(metal = round(Cu_kt,2),
           grade = round(Cu_pct,2),
           recovery = round(Cu_rec,2),
           group = "Copper")

d2 <- d %>% 
 group_by(Year, PeriodID) %>% 
 summarise(metal = round(Au_koz,2),
           grade = round(Au_ppm,2),
           recovery = round(Au_rec,2),
           group = "Gold")

phase_tbl <- Plan_8Q3_a %>% 
 group_by(Phase, PeriodID) %>% 
 mutate(Area = ifelse((grepl("Phs", Phase) | grepl("UG", Phase)) & (Source == "EX_PIT" | Source == "UG"), paste(Phase), "Rehandle")) %>%  
 group_by(Year, PeriodID, Area, Level) %>% 
 filter(Area != "UG" & Area != "Rehandle") %>% 
 summarise(TMM = round(sum(Tonnage),0))

tmp_c <- phase_tbl [,c(1:3)]
tmp_c <- merge(tmp_c, data.frame(Level = seq(720,1170,by=15)), all=T )

phase_tbl <- merge(phase_tbl, tmp_c, by=c("Year", "PeriodID", "Area", "Level"), all=T)
phase_tbl <- phase_tbl %>% replace(.,is.na(.),0)
phase_tbl$color <- ifelse(phase_tbl$Area == "Phs04a", "#FF0000",
                          ifelse(phase_tbl$Area == "Phs04b", "#FF7F00",
                                 ifelse(phase_tbl$Area == "Phs04b-n", "#FFFF00",
                                        ifelse(phase_tbl$Area == "Phs05", "#00FF00",
                                               ifelse(phase_tbl$Area == "Phs06", "#007FFF",
                                                      ifelse(phase_tbl$Area == "Phs06b", "#00FFFF",
                                                             ifelse(phase_tbl$Area == "Phs07", "#7F00FF","#000000")))))))


library(ggplot2)
library(ggiraph)
mytheme <- theme_minimal() + theme(panel.grid = element_blank())
ttip1 = paste0(Area,"<br/>", "Ktonnes = ",round(TMM,0), "<br/>", "Elevation = ", Level)

jColors <- phase_tbl$color
names(jColors) <- phase_tbl$Area
head(jColors)

gga <- phase_tbl %>% 
 # filter(PeriodID %in% c("Dec 2019", "Jan 2020")) %>% 
 filter(Year==2019) %>% 
 group_by(Area, Level) %>% 
 summarise(TMM = round(sum(TMM)/10^3,0)) %>% 
 filter(TMM>0) %>% 
 ggplot(aes(x=Area, y = Level))+
 geom_tile_interactive(aes(fill=Area, width=0.7, height=14, 
                           tooltip = paste0(Area,"<br/>", "Ktonnes = ",format(round(TMM,0), big.mark = ","), "<br/>", "Elevation = ", Level)))+
 # geom_text(aes(label=TMM))+
 # scale_fill_gradientn(colours = c("white", "red"),
 #                       values = scales::rescale(c(0,1,1000000)),
 #                       guide = "colorbar",limits=c(0,1000000))+
 scale_y_continuous(breaks=seq(720,1170,by=15))+
 mytheme+
 guides(fill=FALSE)+
 labs(x= "",
      y= "Elevation")

ggiraph(code = {print(gga)}, tooltip_opacity = 0.5 , selection_type = "single")


saveRDS(a, file = "C:/Rama/R/github/rdprad/R-projects/R/MinePlan_Dashboard/objects/a.rds")
saveRDS(b, file = "C:/Rama/R/github/rdprad/R-projects/R/MinePlan_Dashboard/objects/b.rds")
saveRDS(d, file = "C:/Rama/R/github/rdprad/R-projects/R/MinePlan_Dashboard/objects/d.rds")
saveRDS(d1, file = "C:/Rama/R/github/rdprad/R-projects/R/MinePlan_Dashboard/objects/d1.rds")
saveRDS(d2, file = "C:/Rama/R/github/rdprad/R-projects/R/MinePlan_Dashboard/objects/d2.rds")
saveRDS(phase_tbl, file = "C:/Rama/R/github/rdprad/R-projects/R/MinePlan_Dashboard/objects/phase_tbl.rds")
