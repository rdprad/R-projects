library(dplyr)

OC <- read.csv(file.choose(), header = T)
OC[,c(4,5,6)] <- NULL
RSC <- read.csv(file.choose(), header = T)
RSC[,c(4,5,6)] <- NULL
BM_df <- merge(OC, RSC, by=c("xcentre", "ycentre", "zcentre"))

phasename <- read.csv(file = here::here("objects","phasename.csv"), header = T)
phasename <- phasename %>% 
 select(EAST, NORTH, ELEV, PHASE)
colnames(phasename) <- c("xcentre", "ycentre", "zcentre", "Phase")

BM_df <- merge(BM_df, phasename, by = c("xcentre", "ycentre", "zcentre"))
BM_df$PhaseID <- ifelse(BM_df$Phase==42, "Phase 4B",
                        ifelse(BM_df$Phase==62, "Phase 6B",
                               ifelse(BM_df$Phase==4, "Phase 4A",
                                      paste("Phase",BM_df$Phase))))
BM_df$PhaseID <- factor(BM_df$PhaseID, levels = c("Phase 1","Phase 2","Phase 3","Phase 4A","Phase 4B","Phase 5","Phase 6","Phase 6B","Phase 7"))
BM_df$elevation <- BM_df$zcentre - 7.5


#naive naming
# colnames(BM_df)[grepl(c("oc_au","oc_cu","oc_ag","oc_as","oc_nsr","oc_kdphr","oc_sg","ag","as","au1","cu1","density","Phase"), colnames(BM_df))] 
colnames(BM_df)[grepl("^oc_au$", colnames(BM_df))] <- "BAU"
colnames(BM_df)[grepl("^oc_cu$", colnames(BM_df))] <- "BCU"
colnames(BM_df)[grepl("^oc_ag$", colnames(BM_df))] <- "BAG"
colnames(BM_df)[grepl("^oc_as$", colnames(BM_df))] <- "BAS"
colnames(BM_df)[grepl("^oc_nsr$", colnames(BM_df))] <- "BNSR"
colnames(BM_df)[grepl("^oc_kdphr$", colnames(BM_df))] <- "BKDPHR"
colnames(BM_df)[grepl("^oc_sg$", colnames(BM_df))] <- "BSG"
colnames(BM_df)[grepl("^ag$", colnames(BM_df))] <- "AG"
colnames(BM_df)[grepl("^as$", colnames(BM_df))] <- "AS"
colnames(BM_df)[grepl("^au1$", colnames(BM_df))] <- "AU"
colnames(BM_df)[grepl("^cu1$", colnames(BM_df))] <- "CU"
colnames(BM_df)[grepl("^density$", colnames(BM_df))] <- "DEN"


BM_filter <- BM_df %>% 
 select(xcentre, ycentre, zcentre, BCU, BAU, BSG, BNSR, BKDPHR, CU, AU, DEN, PhaseID, elevation)

# NSR <- read.csv(file = "C:/Rama/OC_Model_Clone/nsr.csv", header = T)
# NSR$X <- NULL
# colnames(NSR) <- c("xcentre", "ycentre", "zcentre", "NSR")

NSR <- read.csv(file = "C:/Rama/OC_Model_Clone/kdphr.csv", header = T)
NSR$X <- NULL
colnames(NSR) <- c("xcentre", "ycentre", "zcentre", "KDPHR", "NSR")

BM_filter <- merge(BM_filter, NSR, by = c("xcentre", "ycentre", "zcentre"))

saveRDS(BM_filter, here::here("objects", "BM_df.rds"))
