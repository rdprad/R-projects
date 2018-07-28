library(here)
library(RODBCext)
library(dplyr)

phasename <- read.csv(file = here::here("objects","phasename.csv"), header = T)
phasename <- phasename %>% 
 select(EAST, NORTH, ELEV, PHASE)
colnames(phasename) <- c("xcentre", "ycentre", "zcentre", "Phase")

dbhandle <- odbcDriverConnect('driver={SQL Server};server=MNOYTSQLD7;database=OTVulcan;trusted_connection=true')
query <- "SELECT [xworld],[yworld],[zworld],[AU],[CU],[AG],[AS],[BCU],[BAU],[BAG],[BAS],[BSG],[BNSR],[KDPHR] FROM [dbo].[OTBM] WHERE [zworld]>=667.5 and [xworld]>=649895 and [xworld]<=651625 and [yworld]>=4762265 and [yworld]<=4764735"
OCcon_df <- sqlExecute(channel = dbhandle, 
                      query = query,
                      fetch = TRUE,
                      stringsAsFactors = FALSE) 
colnames(OCcon_df)[1:3] <- c("xcentre", "ycentre", "zcentre")

BM_df <- merge(OCcon_df, phasename, by = c("xcentre", "ycentre", "zcentre"))
BM_df$PhaseID <- ifelse(BM_df$Phase==42, "Phase 4B",
                        ifelse(BM_df$Phase==62, "Phase 6B",
                               ifelse(BM_df$Phase==4, "Phase 4A",
                                      paste("Phase",BM_df$Phase))))
BM_df$PhaseID <- factor(BM_df$PhaseID, levels = c("Phase 1","Phase 2","Phase 3","Phase 4A","Phase 4B","Phase 5","Phase 6","Phase 6B","Phase 7"))
BM_df$elevation <- BM_df$zcentre - 7.5
saveRDS(BM_df, here::here("objects", "BM_df.rds"))

rm(BM_df, OCcon_df, phasename, dbhandle, query)
