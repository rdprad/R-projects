TPOH <- function(SPI, CI, MB) {
  ifelse(SPI>0 & SPI<=60, 29320 * CI^0.19 * SPI^-0.36 * MB^-0.24,
                     ifelse(SPI>60,26649 * CI^0 * SPI^-0.208 * MB^-0.24,0))
}

TPOH2 <- function(SPI, CI, MB) {
  ifelse(SPI>0, 29320 * CI^0.19 * SPI^-0.36 * MB^-0.24,0)
}


ggplot(data.frame(SPI=c(20,250)), aes(x=SPI))+
  stat_function(fun = function(x){TPOH(x, 18,15)}, aes(col="bdt37"))+
  stat_function(fun = function(x){TPOH2(x, 18,15)}, aes(col="bdt31"))+
  theme_minimal()+
  labs(col="BDT version",
       y  ="TPOH")

# start ------------
library(dplyr)
library(DBI)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};SERVER=MNOYTSQLD1;database=OpenPit;trusted_connection=true")
year <- substr(Sys.Date(),1,4)
q1 <- tbl(con, "Actual_Data") %>% 
          filter(Year==year) 
SW <- c("1","2","3","4","42","5","TK","HG1","MG1","MG4","LG1")
Qcon <- collect(q1)
Qcon$CONCU <- ifelse(Qcon$CONCU<=18,18,Qcon$CONCU)
Qcon$ConTon <- Qcon$Tonnes*Qcon$RECCU/Qcon$CONCU
Qcon$TPOH <- ifelse(Qcon$SPI>0 & Qcon$SPI<=60, 29320 * Qcon$CI^0.19 * Qcon$SPI^-0.36 * Qcon$MB^-0.24,
                    ifelse(Qcon$SPI>60,26649 * Qcon$CI^0 * Qcon$SPI^-0.208 * Qcon$MB^-0.24,4190))
# Qcon$TPOH <- ifelse(Qcon$SPI>0, 29320 * Qcon$CI^0.19 * Qcon$SPI^-0.36 * Qcon$MB^-0.24,4190) #BDT31
Qcon$HPKT <- ifelse(Qcon$TPOH>0, 1000/Qcon$TPOH, 1000/4190)



OCcon <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};SERVER=MNOYTSQL7;database=MineToMill2;trusted_connection=true")
q2 <- tbl(OCcon, "OC_AllReady_FRONT") %>% 
  select(cutName, Tonnes)
Qcon2 <- collect(q2)
Qcon2$Tonnes <- as.numeric(Qcon2$Tonnes)
# Qcon2$Phase <- as.numeric(Qcon2$Phase)
# Qcon2$BCu <- as.numeric(Qcon2$BCu)
# Qcon2$BAu <- as.numeric(Qcon2$BAu)

merge_Qcon <- merge(Qcon %>% 
                group_by(Source,`Minor Source` ,`Minor Destination`) %>% 
                summarise(MMRS_tonnes = sum(Tonnes),
                          Cu          = weighted.mean(CU, Tonnes),
                          Au          = weighted.mean(AU, Tonnes)),
              Qcon2 %>% 
                rename(Source       = cutName, 
                       OC_inventory = Tonnes), 
              by=c("Source"), all = T)

# merge_Qcon <- merge_Qcon %>% 
#   filter(`Minor Destination` %in% "CRSH")


library(ggiraph)
library(ggplot2)
a <- merge_Qcon %>% 
       filter(`Minor Destination` %in% "CRSH", `Minor Source` %in% c(4,6,42)) %>% 
       ggplot(aes(x=MMRS_tonnes, y=OC_inventory))+
       geom_point_interactive(aes(color=`Minor Source`, size=Au, alpha=0.3,tooltip=paste0("Source=", Source,"<br/>","Au=", round(Au,2),
                                                                                             "<br/>", "MMRS tonnes=", format(round(MMRS_tonnes,0), big.mark=","),
                                                                                             "<br/>", "OC Inventory=", format(round(OC_inventory,0), big.mark=","),
                                                                                             "<br/>", "digging compliance=", round(MMRS_tonnes/OC_inventory,2))))+
       geom_abline(slope = 1, intercept = 0, col="blue")+
       coord_equal(ratio=1)+
       scale_size_continuous(range = c(0, 4))+
       ylim(0,250000)+
       xlim(0,250000)+
       theme_minimal()+
       theme(legend.position = "bottom")+
       guides(size=F, alpha=F)+
       labs(color= "Phase")
  
ggiraph(ggobj = a, tooltip_opacity = 0.4)

b <- merge_Qcon %>% 
       filter(grepl("MG", Source), `Minor Source` %in% c(4,6,42)) %>% 
       ggplot(aes(x=MMRS_tonnes, y=OC_inventory))+
       geom_point_interactive(aes(color=`Minor Source`, size=Au, alpha=0.3,tooltip=paste0("Source=", Source,"<br/>","Au=", round(Au,2),
                                                                                             "<br/>", "MMRS tonnes=", format(round(MMRS_tonnes,0), big.mark=","),
                                                                                             "<br/>", "OC Inventory=", format(round(OC_inventory,0), big.mark=","),
                                                                                             "<br/>", "digging compliance=", round(MMRS_tonnes/OC_inventory,2))))+
       geom_abline(slope = 1, intercept = 0, col="blue")+
       coord_equal(ratio=1)+
       scale_size_continuous(range = c(0, 4))+
       # ylim(0,250000)+
       # xlim(0,250000)+
       theme_minimal()+
       theme(legend.position = "bottom")+
       guides(size=F, alpha=F)+
       labs(color= "Phase")
  
ggiraph(ggobj = b, tooltip_opacity = 0.4)



# Table Summary---------
merge_Qcon %>% 
  filter(`Minor Destination` %in% "CRSH", `Minor Source` %in% c(4,6,42)) %>% 
  mutate(new_OC_tonnes = ifelse(is.na(OC_inventory),0,OC_inventory)) %>% 
  group_by(`Minor Source`) %>% 
  summarise(MMRS      = sum(MMRS_tonnes),
            OC        = sum(OC_inventory, na.rm = T),
            MMRS_Cu   = weighted.mean(Cu,MMRS_tonnes),
            MMRS_Au   = weighted.mean(Au,MMRS_tonnes),
            OC_Cu     = weighted.mean(Cu,new_OC_tonnes, na.rm = T),
            OC_Au     = weighted.mean(Au,new_OC_tonnes, na.rm = T))

merge_Qcon %>% 
  filter(`Minor Destination` %in% "CRSH", `Minor Source` %in% c(4,6,42)) %>% 
  mutate(new_OC_tonnes = ifelse(is.na(OC_inventory),0,OC_inventory)) %>% 
  summarise(`Minor Source`     = "TOTAL",
            MMRS      = sum(MMRS_tonnes),
            OC        = sum(OC_inventory, na.rm = T),
            MMRS_Cu   = weighted.mean(Cu,MMRS_tonnes, na.rm = T),
            MMRS_Au   = weighted.mean(Au,MMRS_tonnes, na.rm = T),
            OC_Cu     = weighted.mean(Cu,new_OC_tonnes, na.rm = T),
            OC_Au     = weighted.mean(Au,new_OC_tonnes, na.rm = T))



