rastlist <- list.files(here::here("outputs/tif_outputs"), pattern='.tif', 
                       all.files=TRUE, full.names=FALSE)

allrasters <- lapply(rastlist, raster)

#first import all files in a single folder as a list 
rastlist <- list.files(path = here::here("outputs/tif_outputs"), pattern='.tif', 
                       all.files=TRUE, full.names=FALSE)

rastlist <- unlist(lapply(1:length(rastlist), function(x){
  paste0(here::here("outputs/tif_outputs"),"/",rastlist[x])
}))

allrasters <- stack(rastlist)

df <-  data.frame(getValues(allrasters))
df  <- data.frame(ID = seq(from = 1, to = nrow(df),by=1),
                  Rthr = df$richness_initTHR,
                  Rnothr = df$richness_initNonTHR,
                  Rnostatus = df$richness_initNoStatus,
                  Rfinalthr = df$richness_finalTHR,
                  Rfinalnothr = df$richness_finalNonTHR,
                  Rfinalnostatus = df$richness_finalNoStatus
                  )



#df  <-  merge(Zrank_main,df,by = "ID",all.x=T)

df  <-  merge(Zrank_main,df,by = "ID",all=T)
df$DeltaRank   <-  df$rankSc2-df$rankSc1
df$DeltaThr  <-   df$Rfinalthr-df$Rthr



df  <-  merge(df,all_geo_res[,c("ID","MPA")],by = "ID",all.x=T)

all_geo_res2 <- df
all_geo_res2$richness <-  all_geo_res2$Rthr+all_geo_res2$Rnothr+all_geo_res2$Rnostatus
save(all_geo_res2,file=here::here("outputs","all_geo_res2.RData"))


