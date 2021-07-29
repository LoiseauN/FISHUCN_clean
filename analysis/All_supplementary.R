library(raster)
load("PA_Coast.RData")


dat_network




dat_network$species <- str_replace(dat_network$species,"-", "_")

Thr_IUCN <- cbind(PA_Coast[,c(1:2)],
            PA_Coast[,colnames(PA_Coast) %in% subset(dat_network,dat_network$IUCN_alone == "Threatened")$species])

Thr_IUCN_predict<- cbind(PA_Coast[,c(1:2)],
                         PA_Coast[,colnames(PA_Coast) %in% subset(dat_network,dat_network$IUCN_final == "Threatened")$species])

Non_Thr_IUCN <- cbind(PA_Coast[,c(1:2)],
                      PA_Coast[,colnames(PA_Coast) %in% subset(dat_network,dat_network$IUCN_alone == "Non Threatened")$species])

Non_Thr_IUCN_predict<- cbind(PA_Coast[,c(1:2)],
                             PA_Coast[,colnames(PA_Coast) %in% subset(dat_network,dat_network$IUCN_final == "Non Threatened")$species])

NoStatus_IUCN <- cbind(PA_Coast[,c(1:2)],
                       PA_Coast[,colnames(PA_Coast) %in% subset(dat_network,dat_network$IUCN_alone == "No Status")$species])

NoStatus_IUCN_predict<- cbind(PA_Coast[,c(1:2)],
                              PA_Coast[,colnames(PA_Coast) %in% subset(dat_network,dat_network$IUCN_final == "No Status")$species])




Thr_IUCN <- data.frame(Thr_IUCN[,c(1,2)], NbS = apply(Thr_IUCN[,-c(1,2)],1,sum))
Thr_IUCN_raster <- rasterFromXYZ(Thr_IUCN)

Thr_IUCN_predict <- data.frame(Thr_IUCN_predict[,c(1,2)], NbS = apply(Thr_IUCN_predict[,-c(1,2)],1,sum))
Thr_IUCN_predict_raster <- rasterFromXYZ(Thr_IUCN_predict)

Non_Thr_IUCN <- data.frame(Non_Thr_IUCN[,c(1,2)], NbS = apply(Non_Thr_IUCN[,-c(1,2)],1,sum))
Non_Thr_IUCN_raster <- rasterFromXYZ(Non_Thr_IUCN)

Non_Thr_IUCN_predict <- data.frame(Non_Thr_IUCN_predict[,c(1,2)], NbS = apply(Non_Thr_IUCN_predict[,-c(1,2)],1,sum))
Non_Thr_IUCN_predict_raster <- rasterFromXYZ(Non_Thr_IUCN_predict)

NoStatus_IUCN <- data.frame(NoStatus_IUCN[,c(1,2)], NbS = apply(NoStatus_IUCN[,-c(1,2)],1,sum))
NoStatus_IUCN_raster <- rasterFromXYZ(NoStatus_IUCN)

NoStatus_IUCN_predict <- data.frame(NoStatus_IUCN_predict[,c(1,2)], NbS = apply(NoStatus_IUCN_predict[,-c(1,2)],1,sum))
NoStatus_IUCN_predict_raster <- rasterFromXYZ(NoStatus_IUCN_predict)

par(mfrow=c(3,2))
plot(Thr_IUCN_raster,zlim=c(0,max(c(maxValue(Thr_IUCN_raster),maxValue(Thr_IUCN_predict_raster)))),
     main = "Threatened by IUCN")
plot(Thr_IUCN_predict_raster,zlim=c(0,max(c(maxValue(Thr_IUCN_raster),
                                            maxValue(Thr_IUCN_predict_raster)))),
     main = "Threatened by predict & IUCN")

plot(Non_Thr_IUCN_raster,zlim=c(0,max(c(maxValue(Non_Thr_IUCN_raster),
                                        maxValue(Non_Thr_IUCN_predict_raster)))),
     main = "Non threatened by IUCN")
plot(Non_Thr_IUCN_predict_raster,zlim=c(0,max(c(maxValue(Non_Thr_IUCN_raster),
                                                maxValue(Non_Thr_IUCN_predict_raster)))),
     main = "Non threatened by predict & IUCN")

plot(NoStatus_IUCN_raster,zlim=c(0,max(c(maxValue(NoStatus_IUCN_raster),
                                         maxValue(NoStatus_IUCN_predict_raster)))),
     main = "No status by IUCN")
plot(NoStatus_IUCN_predict_raster,zlim=c(0,max(c(maxValue(NoStatus_IUCN_raster),
                                                 maxValue(NoStatus_IUCN_predict_raster)))),
     main = "No status by predict & IUCN")







##########

files <- list.files("~/Documents/Postdoc MARBEC/FISHUCN/Clean/rasterFish",full.names=TRUE)
#names_sp <- list.files("~/Documents/Postdoc MARBEC/FISHUCN/Clean/rasterFish")
#names_sp <- str_replace(names_sp, "_", "-")
#names_sp <- str_replace(names_sp, ".tif", "")



rastStack <- raster::stack(files)



Stack_Thr_IUCN <- subset(rastStack, which(names(rastStack) %in%
                                 subset(dat_network,dat_network$IUCN_alone == "Threatened")$species))

rm_ord <- calc(Stack_Thr_IUCN,function(x, ...) sum(x))

s2 <- stack(s, r_start, r_end)
sum_time <- function(x) {sum(x[x[6]:x[7]], na.rm = T)}

system.time(
  output <- calc(s2, fun = sum_time)
)
Stack_Thr_IUCN <- Stack_Thr_IUCN[1:3]


Stack_Thr_IUCN <-  stackApply(Stack_Thr_IUCN,1,sum)



Stack_Non_Thr_IUCN <- subset(rastStack, which(names(rastStack) %in%
                                            subset(dat_network,dat_network$IUCN_alone == "Non Threatened")$species))
Stack_Non_Thr_IUCN <-  stackApply(Stack_Non_Thr_IUCN,3,sum)

Stack_Thr_Predict <- subset(rastStack, which(names(rastStack) %in%
                                            subset(dat_network,dat_network$IUCN_final == "Threatened")$species))
Stack_Thr_Predict <-  stackApply(Stack_Thr_Predict,3,sum)

Stack_Non_Thr_Predict <- subset(rastStack, which(names(rastStack) %in%
                                                subset(dat_network,dat_network$IUCN_final == "Non Threatened")$species))
Stack_Non_Thr_Predict <-  stackApply(Stack_Non_Thr_Predict,1,sum)

tryCatch({
  system.time({
    no_cores <- parallel::detectCores() - 2
    raster::beginCluster(no_cores)
    myFun <- function(x, ...) {
      sum(!is.na(x))
    }
    r_week <- raster::clusterR(Stack_Thr_IUCN, stackApply, args=list( fun = myFun, na.rm = TRUE,progress='text'))
    raster::endCluster()})
}, error = function(e) {
  raster::endCluster()
  return(e)
}, finally = {
  try(raster::endCluster())
})

MAP_Thr_IUCN <- ggplot() +
  geom_tile(data=RankDiff,aes(x = x, y = y, fill = Diff))+
  scale_fill_gradient2(low = "blue", midpoint = 0, mid = "yellow", high = "red", name = "Diffence") +
  ggtitle("Difference Ranking IUCN/IUCN + Predict")+
  theme_bw()+
  xlab("")+ylab("")
