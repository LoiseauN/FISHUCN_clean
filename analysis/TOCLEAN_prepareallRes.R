rastlist <- list.files(here::here("outputs/tif_outputs"), pattern='.tif', 
                       all.files=TRUE, full.names=FALSE)

allrasters <- lapply(rastlist, raster)

#first import all files in a single folder as a list 
rastlist <- list.files(path = here::here("outputs/tif_outputs"), pattern='.tif', 
                       all.files=TRUE, full.names=FALSE)

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

head(all_geo_res)

df  <-  merge(Zrank_main,df,by = "ID",all.x=T)
df$DeltaRank   <-  df$rankSc2-df$rankSc1
df$DeltaThr  <-   df$Rfinalthr-df$Rthr
richness <-  df$Rthr+df$Rnothr+df$Rnostatus


df  <-  merge(df,all_geo_res[,c("ID","MPA")],by = "ID",all.x=T)

all_geo_res
#import all raster files in folder using lapply

#to check the index numbers of all imported raster list elements
allrasters


Zrank_main$DeltaRank <- Zrank_main$rankSc2-Zrank_main$rankSc1


ggsave(file = here::here("figures/test2.png"),fig_rank,width = 12, height = 12, units= "in",dpi= 300)



df_richnessTHR_final <- data.frame(ID = seq(from = 1, to = (richnessTHR_final@ncols*richnessTHR_final@nrows),by=1),
                                   THR_final =  getValues(richnessTHR_final))




  
df$DeltaRank <- df$rankSc2-df$rankSc1


plot(df$DeltaRank,df)