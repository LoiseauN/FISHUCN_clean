#' The make.R must be run first
#' 
#' 
#' 
## Extract IUCN status ----
mammals_status    <- rredlist::rl_comp_groups("mammals", 
  key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")

birds_status      <- rredlist::rl_comp_groups("birds", 
  key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")

amphibians_status <- rredlist::rl_comp_groups("Amphibians", 
  key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")


## Download Phylopic silhouettes (with License 1.0 and No Copyright) ----
mammals_pic    <- rphylopic::image_data("8cad2b22-30d3-4cbd-86a3-a6d2d004b201", 
                                        size = "512")[[1]]

birds_pic      <- rphylopic::image_data("34d9872c-b7d0-416f-8ac6-1f9f952982c8", 
                                        size = "512")[[1]]

fish_pic       <- rphylopic::image_data("86c40d81-2613-4bb4-ad57-fb460be56ae5", 
                                        size = "512")[[1]]

amphibians_pic <- rphylopic::image_data("cd0cdc36-ecfa-414f-af87-1b5e0ec0c69b", 
                                        size = "512")[[1]]



#'------------------------------------------------------------------------------@Comparisonothertaxa

data_4_taxa <- data.frame(taxa   = c(rep("mammals", nrow(mammals_status$result)),
                                     rep("birds", nrow(birds_status$result)),
                                     rep("amphibians", nrow(amphibians_status$result)),
                                     rep("fishes", nrow(FB_final))),
                          status = as.factor(c(mammals_status$result$category,
                                                birds_status$result$category,
                                                amphibians_status$result$category,
                                                as.character(FB_final$IUCN))))

levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("LR/cd", "LR/nt", "nt","NT", "LC","NThr")] <- "Non Threatened"

levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("DD",  "NA")] <- "No Status"

data_4_taxa[is.na(data_4_taxa$status),]$status <- "No Status"

levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("CR", "EN", "EW","VU","Thr")] <- "Threatened"

data_4_taxa <- subset(data_4_taxa,data_4_taxa$status != "EX")

data_4_taxa <- as.matrix(table(data_4_taxa))
data_4_taxa <- (data_4_taxa/apply(data_4_taxa,1,sum))*100

data_4_taxa <- transform(data_4_taxa,
                         status = factor(status, c("Threatened",
                                                   "Non Threatened",
                                                   "No Status")))
data_4_taxa <-na.omit(data_4_taxa)
data_4_taxa <- transform(data_4_taxa,
                         taxa = factor(taxa, c("birds",
                                               "mammals",
                                               "amphibians",
                                               "fishes")))

fig1 <- ggplot(data_4_taxa, aes(fill=status, y=Freq, x=taxa)) + 
  geom_bar(position="stack", stat="identity",color="grey20") +
  scale_fill_manual(values = c("#FC4E07","#00AFBB", "#E7B800"), name = "IUCN status", 
                    guide = guide_legend(reverse = TRUE))+
  theme_bw() +
  xlab("Taxa")+ylab("Percentage")+
  add_phylopic(birds_pic,     x = 1, y = 50, ysize = 12, alpha = 1)+
  add_phylopic(mammals_pic,   x = 2, y = 50, ysize = 10, alpha = 1)+
  add_phylopic(amphibians_pic,x = 3, y = 50, ysize = 10, alpha = 1)+
  add_phylopic(fish_pic,      x = 4, y = 50, ysize = 8, alpha = 1) +
  theme( axis.text=element_text(size=16),axis.title=element_text(size=18,face="bold"))
ggsave(file = here::here("figures/Figure1.png"),fig1,width = 12, height = 12, units= "in",dpi= 300)


#'---------------------------------------------------------------------@variable_importance





partial_plot <- var_partial(data =  data_noNA,
                            var = c("DistrArea" , "Max_length"),
                            names = c("Range size (log)","Max Length (log)")) 


importance_plot = var_imp(test_IUCN[[1]])      

importance_plot <- importance_plot + annotation_custom(ggplotGrob(partial_plot[[1]]), xmin = 6, xmax = 11, 
                     ymin = 20, ymax = 35.65)

fig2 <- importance_plot + annotation_custom(ggplotGrob(partial_plot[[2]]), xmin = 1, xmax = 6, 
                                                       ymin = 20, ymax = 35.65)



ggsave(file = here::here("figures/Figure2.png"),fig2,width = 12, height = 8, units= "in",dpi= 300)


#'---------------------------------------------------------------------@ResultsPrediction

#Network
addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}



dat_network <- data.frame(data_zonation[,c("species","IUCN_cat","predict_complementary")])

dat_network <- dat_network[dat_network$species %in%MPA_Protect$species,]


dat_network <- addLevel(dat_network, "Threatened")
dat_network <- addLevel(dat_network, "Non Threatened")
dat_network <- addLevel(dat_network, "No Status")


for (i in 1:ncol(dat_network)){dat_network[,i] <- as.factor(as.character(dat_network[,i]))}


 dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c("Thr"), 
                                  to = c("Threatened")))

dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c("NThr"), 
                                  to = c("Non Threatened")))


#dat_network<-as.data.frame(sapply(dat_network,
 #                                 mapvalues, from = c("LC","NT","nt","NC"), 
  #                                to = c("Non Threatened","Non Threatened",
   #                                      "Non Threatened","Non Threatened")))




dat_network$IUCN_final <- NA

for (i in 1:nrow (dat_network)){
  if(is.na(dat_network$IUCN_cat[i])){ dat_network$IUCN_final[i]=dat_network$predict[i]
  
  }else{
    
    dat_network$IUCN_final[i]=dat_network$IUCN_cat[i]
    
  }
}


dat_network[is.na(dat_network$IUCN_final),]$IUCN_final <- "No Status"
dat_network[is.na(dat_network$IUCN_cat),]$IUCN_cat <- "No Status"

for (i in 1:nrow(dat_network)){
  if(is.na(dat_network$predict_complementary[i]) & dat_network$IUCN_cat[i] == "No Status") {
    dat_network$predict_complementary[i] <- "No Status"
  
      }
  }

pos <- which(dat_network$'IUCN_final' == "NaN")
if (length(pos)) dat_network[pos, "IUCN_final"] <- "No Status"


df <- data.frame(id = rep(dat_network$species,2),
                 stage = as.factor(c(rep("Before Prediction",nrow(dat_network)), rep("After Prediction",nrow(dat_network)))),
                 group = as.factor(c(dat_network$IUCN_cat,dat_network$IUCN_final)))

df <- transform(df,
                group = factor(group, rev(levels(group))))
df <- transform(df,
                stage = factor(stage, rev(levels(stage))))

plot_net <- 
  ggplot(df, aes(x = stage, stratum = group, alluvium = id, fill = group, label = stage)) +
  scale_x_discrete(expand = c(.15, .15)) +
  geom_flow(color="white") +
  scale_fill_manual(values = c("#FC4E07","#00AFBB", "#E7B800","pink"), name = "IUCN status", 
                    guide = guide_legend(reverse = TRUE)) +
  geom_stratum(alpha = 1,color="white") +
  geom_text(stat = "stratum",
            aes(label = percent(after_stat(prop), accuracy = .1)))+
  theme_bw()+
  xlab("") +ylab("Number of species")

print(plot_net)


#ggsave(file = here::here("figures/Figure3.png"),plot_net, width = 12, height = 12, units= "in",dpi= 300)
ggsave(file = here::here("figures/Figure2.png"),fig2,width = 12, height = 8, units= "in",dpi= 300)


#'---------------------------------------------------------------------@Percentagegainmodel
#'( ( valeur d'arrivée - valeur de départ ) / valeur de départ ) x 100
gainThr <- ((table(dat_network$IUCN_cat)[1]-table(dat_network$IUCN_final)[1])/table(dat_network$IUCN_cat)[1])*100
gainNThr <- ((table(dat_network$IUCN_cat)[3]-table(dat_network$IUCN_final)[3])/table(dat_network$IUCN_cat)[3])*100

mean(do.call(rbind,phylo_D_Thr)[,1])
sd(do.call(rbind,phylo_D_Thr)[,1])

mean(do.call(rbind,phylo_D_NonThr)[,1])
sd(do.call(rbind,phylo_D_NonThr)[,1])

mean(do.call(rbind,phylo_D_nostatus)[,1])
sd(do.call(rbind,phylo_D_nostatus)[,1])

#'---------------------------------------------------------------------@Checkperfamily
#'( ( valeur d'arrivée - valeur de départ ) / valeur de départ ) x 100
res <- merge(dat_network, FB_final[,c("Genus","Family")], by.x="species", by.y="row.names")



res$predict_complementary <- as.factor(res$predict_complementary )
res <- as.data.frame.matrix(t(table(res$predict_complementary,res$Family)))

res$percenTHR <- NA

for (i in 1:nrow(res)){
  res[i,4] <- res[i,3]/sum(res[i,1],res[i,2],res[i,3])
}

family <- rownames(res)
res <- do.call(data.frame, lapply(res, function(x) {
  replace(x, is.infinite(x), NA)
})
)

rownames(res) <- family 

res <-  res[order(res$percenTHR,decreasing = T),]

#FAMILY TO CHECK


#'---------------------------------------------------------------------@Protectionanalyses

MPA_Protect <- merge(MPA_Protect,dat_network,by="species")
MPA_Protect$IUCN_final <- as.factor(MPA_Protect$IUCN_final)
MPA_Protect$species <- as.factor(MPA_Protect$species)
MPA_Protect$predictORiucn <- NA
for (i in 1:nrow(MPA_Protect)){
  print(i)
 
  if(is.na(MPA_Protect$predict_complementary[i])) {MPA_Protect$predictORiucn[i] <- "IUCN" 
  }
  else{
    MPA_Protect$predictORiucn[i] <- "Predicted" 
  }
  
  
}

MPA_Protect$LOGIT_Target_achievement_I_IV <- log10(MPA_Protect$Target_achievement_I_IV+1)

Target_all <-   ggstatsplot::ggbetweenstats(
  data = MPA_Protect, #data_protected,
  x = IUCN_final,
  y = LOGIT_Target_achievement_I_IV
)

Target_all <-  Target_all +
  labs(
    x = "IUCN Status",
    y = "log10 (Target achievement MPA (I - IV) +1)"
  ) +
  scale_color_manual(values=c("#00AFBB", "#E7B800","#FC4E07"))




ggsave(file = here::here("figures/Figure4.png"),Target_all, width = 12, height = 12, units= "in",dpi= 300)


#'---------------------------------------------------------------------@UNpredictedspecies
noprediction <- FB_final[rownames(FB_final) %in% all_predict[is.na(all_predict$predict_complementary),]$species,]
sum(is.na(FB_final$K))

#'---------------------------------------------------------------------@zonationanalyses
#Zrank_main$diff <- Zrank_main$rankSc2-Zrank_main$rankSc1

#fig_rank_hex <- ggplot(Zrank_main, aes(x=rankSc1, y=rankSc2) ) +
#  geom_hex(bins = 100) +
#  scale_fill_continuous(type = "viridis") +
#  theme_bw() + xlab("IUCN only")+ ylab("IUCN + Predicted")
#ggsave(file = here::here("figures/fig_rank_hex.png"),width = 12, height = 12, units= "in",dpi= 300)
#all_geo_res <- all_geo_res[,-c(10,11)]
#all_geo_res <- merge(all_geo_res,Zrank_main,by = "ID",all.x=T)
#all_geo_res$DeltaRank <- all_geo_res$rankSc2-all_geo_res$rankSc1
  
  
all_geo_res <- all_geo_res[order(all_geo_res$richness, decreasing=FALSE), ]
all_geo_res <- na.omit(all_geo_res)
all_geo_res <- all_geo_res[all_geo_res$richness  > 0,]

fig_rank <- ggplot(all_geo_res, aes(x=rankSc1, y=rankSc2, color = log10(richness))) +
  geom_point(size=2.5,alpha = 0.3,shape=16) + 
  scale_color_distiller(palette = "Spectral")+
  theme_bw() + xlab("IUCN")+ ylab("IUCN + Predicted") +
  geom_abline(slope=1, intercept = 0)
ggsave(file = here::here("figures/Figure5.png"),fig_rank,width = 12, height = 12, units= "in",dpi= 300)

fig_rank <- ggplot(all_geo_res, aes(x=richness, y=DeltaThr, color = log10(richness))) +
  geom_point(size=2.5,alpha = 0.3,shape=16) + 
  scale_color_distiller(palette = "Spectral")+
  theme_bw() + xlab("IUCN")+ ylab("IUCN + Predicted") 
ggsave(file = here::here("figures/test.png"),fig_rank,width = 12, height = 12, units= "in",dpi= 300)

fig_rank <- ggplot(all_geo_res, aes(x=Rnothr, y=Rfinalnothr, color = log10(richness))) +
  geom_point(size=2.5,alpha = 0.3,shape=16) + 
  scale_color_distiller(palette = "Spectral")+
  theme_bw() + xlab("IUCN")+ ylab("IUCN + Predicted") 
ggsave(file = here::here("figures/test2.png"),fig_rank,width = 12, height = 12, units= "in",dpi= 300)

#NOT GOOD
#'---------------------------------------------------------------------@Otherpresentation
#test <- na.omit(all_geo_res[sample(nrow(all_geo_res), 10000), ]) 

#test$pos <- NA
#for (i in 1:nrow(test)){
#  print(i)
#  if(test$DeltaRank[i]>0) test$pos[i]=2
#  if(test$DeltaRank[i]<0) test$pos[i]=1
#  if(test$DeltaRank[i]==0) test$pos[i]=3
#}


#fig_rank <- ggplot(test, aes(log10(richness), DeltaRank)) +
#  geom_point(aes(fill = factor(sign(DeltaRank))), size = 3,shape=21) +
#  geom_smooth(aes(colour = factor(pos)))+
#  scale_fill_manual(values =c("dodgerblue2","grey","chocolate1"))+
#  scale_colour_manual(values = c("black","black"))+
#    theme_bw()+
#  theme(legend.position = "none")+
#  ylim(-max(abs(test$DeltaRank)),max(abs(test$DeltaRank)))
# ggsave(file = here::here("figures/Figure5bis.png"),fig_rank,width = 12, height = 12, units= "in",dpi= 300)


#'---------------------------------------------------------------------@MAP
#all_geo_res
mask.full=raster::raster(here::here("data","mask.full.tif"))
mask.full.polygon <- sf::st_as_sf(as.point = F)





#--- Diff ranking  WHITE HOLE ARE MPA

var = c("DeltaRank","DeltaStd_R")
all_map <- lapply(1:length(var),function(x){
 
  mask = mask.full
  df <- all_geo_res[,var[x]]
  df[is.na(df)] <- 0
  mask[all_geo_res$ID] = df
  
  #raster to stars
  mask <- stars::st_as_stars(mask)
  mask.full.polygon <- sf::st_as_sf(mask,as.point = F)
  mask.full.polygon <-  fortify(mask.full.polygon)
  #df <-as.data.frame(rasterToPoints(mask))
  #colnames(df)[3] <- "value"
  
  if (var[x] =="DeltaRank" )  {  title =  "Difference Ranking IUCN/IUCN + Predict" }  
 
 
    
  if (var[x] =="DeltaRank" ){
     map <- ggplot(data = mask.full.polygon) +
      geom_sf(aes(fill = mask.full), color = NA)+#(value/max(value))
     scale_fill_distiller(palette = "RdBu")+
     #ggtitle(title)+
     theme_bw()+
     xlab("")+ylab("")
    
     ggsave(file = here::here("figures/map_test.png"),map,width = 8, height = 12, units= "in",dpi= 300)
     

     
 # map <- ggplot() +
  #  geom_raster(data=df,aes(x = x, y = y, fill = value))+#(value/max(value))
   # scale_fill_distiller(palette = "RdBu")+
  #  ggtitle(title)+
   # theme_bw()+
  #  xlab("")+ylab("")
  


}
})

map <- marrangeGrob(all_map,ncol=1,nrow=2)
ggsave(file = here::here("figures/Figure6.png"),map,width = 8, height = 12, units= "in",dpi= 300)



#'---------------------------------------------------------------------@MapRichess
var = c("Rthr","Rnothr","Rnostatus","Rfinalthr","Rfinalnothr","Rfinalnostatus")

all_map <- lapply(1:length(var),function(x){
  
  mask = mask.full
  df <- all_geo_res[,var[x]]
  df[is.na(df)] <- 0
  mask[all_geo_res$ID] = df
  
  df <-as.data.frame(rasterToPoints(mask))
  colnames(df)[3] <- "value"
  
  if (var[x] =="Rthr" )  {  title =  "IUCN Threatened"  
  map <- ggplot() +
    geom_raster(data=df,aes(x = x, y = y, fill = value))+
    scale_fill_distiller(palette = "Spectral",
                         limits = c(min(c(all_geo_res$Rthr,all_geo_res$Rfinalthr),na.rm=T), max(c(all_geo_res$Rthr,all_geo_res$Rfinalthr),na.rm=T)))+
    ggtitle(title)+
    theme_bw()+
    xlab("")+ylab("")} 
  
  else if (var[x] =="Rnothr" )  {  title =  "IUCN Non-Threatened"  
  map <- ggplot() +
    geom_raster(data=df,aes(x = x, y = y, fill = value))+
    scale_fill_distiller(palette = "Spectral",
                         limits = c(min(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T), max(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T)))+
    ggtitle(title)+
    theme_bw()+
    xlab("")+ylab("")
  } 
  
  else if  (var[x] =="Rnostatus" )  {  title =  "IUCN No-Status"  
  map <- ggplot() +
    geom_raster(data=df,aes(x = x, y = y, fill = value))+
    scale_fill_distiller(palette = "Spectral",
                         limits = c(min(c(all_geo_res$Rnostatus,all_geo_res$Rfinalnostatus),na.rm=T), max(c(all_geo_res$Rnostatus,all_geo_res$Rfinalnostatus),na.rm=T)))+
    ggtitle(title)+
    theme_bw()+
    xlab("")+ylab("")
  } 
  
  else if  (var[x] =="Rfinalthr" )  {  title =  "IUCN + predicted  Threatened"  
  map <- ggplot() +
    geom_raster(data=df,aes(x = x, y = y, fill = value))+
    scale_fill_distiller(palette = "Spectral",
                         limits = c(min(c(all_geo_res$Rthr,all_geo_res$Rfinalthr),na.rm=T), max(c(all_geo_res$Rthr,all_geo_res$Rfinalthr),na.rm=T)))+
    ggtitle(title)+
    theme_bw()+
    xlab("")+ylab("")
  }
  
  else if  (var[x] =="Rfinalnothr" )  {  title =  "IUCN + predicted  Non-Threatened"   
  map <- ggplot() +
    geom_raster(data=df,aes(x = x, y = y, fill = value))+
    scale_fill_distiller(palette = "Spectral",
                         limits = c(min(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T), max(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T)))+
    ggtitle(title)+
    theme_bw()+
    xlab("")+ylab("")
  }
  
  else if  (var[x] =="Rfinalnostatus" )  {  title =  "IUCN + predicted  No-Status"  
  map <- ggplot() +
    geom_raster(data=df,aes(x = x, y = y, fill = value))+
    scale_fill_distiller(palette = "Spectral",
                         limits = c(min(c(all_geo_res$Rnostatus,all_geo_res$Rfinalnostatus),na.rm=T), max(c(all_geo_res$Rnostatus,all_geo_res$Rfinalnostatus),na.rm=T)))+
    ggtitle(title)+
    theme_bw()+
    xlab("")+ylab("")
  } 
  

})



map <- marrangeGrob(all_map,ncol=2,nrow=3)
ggsave(file = here::here("figures/FigureRichess_status.png"),map,width = 12, height = 8, units= "in",dpi= 300)

#'---------------------------------------------------------------------@DistributionThr+Rank

#'---------------------------------------------------------------------@MapofpercentageTHR
all_geo_res$PercentageTHR_before <- all_geo_res$Rthr/all_geo_res$richness
all_geo_res$PercentageTHR_after <- all_geo_res$Rfinalthr/all_geo_res$richness

all_geo_res[is.na(all_geo_res$PercentageTHR_before),]$PercentageTHR_before <- 0
all_geo_res[is.na(all_geo_res$PercentageTHR_after),]$PercentageTHR_after <- 0

var = c("PercentageTHR_before","PercentageTHR_after")
all_map <- lapply(1:length(var),function(x){
  
  mask = mask.full
  df <- all_geo_res[,var[x]]
  df[is.na(df)] <- 0
  mask[all_geo_res$ID] = df
  
  df <-as.data.frame(rasterToPoints(mask))
  colnames(df)[3] <- "value"

    map <- ggplot() +
      geom_raster(data=df,aes(x = x, y = y, fill = value))+#(value/max(value))
      scale_fill_distiller(palette = "RdBu")+
        theme_bw()+
      xlab("")+ylab("")
} )
    
    
 




#'---------------------------------------------------------------------@MapofDeltaTHR_NONTHR
all_geo_res$DeltaNonThr <- all_geo_res$Rfinalnothr-all_geo_res$Rnothr
all_geo_res$DeltaStd_NonThr_R <- all_geo_res$DeltaNonThr/all_geo_res$richness
var = c("DeltaThr","DeltaNonThr", "DeltaStd_R","DeltaStd_NonThr_R")

all_map <- lapply(1:length(var),function(x){
  
  mask = mask.full
  df <- all_geo_res[,var[x]]
  df[is.na(df)] <- 0
  mask[all_geo_res$ID] = df
  
  df <-as.data.frame(rasterToPoints(mask))
  colnames(df)[3] <- "value"
  
  if (var[x] =="DeltaThr" )  {  title =  "Difference Richness Threatened" }  
  if (var[x] =="DeltaNonThr" )  {  title =  "Difference Richness Non-Threatened" }  
  if (var[x] =="DeltaStd_R" )  {  title =  "Difference Standardized Richness Threatened" }  
  if (var[x] =="DeltaStd_NonThr_R" )  {  title =  "Difference Standardized Richness Non-Threatened" }  
    
  map <- ggplot() +
      geom_raster(data=df,aes(x = x, y = y, fill = value))+
      scale_fill_distiller(palette = "Spectral")+
      ggtitle(title)+
      theme_bw()+
      xlab("")+ylab("")

})

map <- marrangeGrob(all_map,ncol=2,nrow=2)
ggsave(file = here::here("figures/FigureDeltaThr_NonTHR.png"),map,width = 12, height = 8, units= "in",dpi= 300)

#'---------------------------------------------------------------------@DistributionThr+Rank


a <- ggplot(all_geo_res, aes(x=DeltaRank)) + geom_histogram(color= "#E69F00",fill="#E69F00", alpha=0.5)+ 
  geom_vline(aes(xintercept=median(DeltaRank,na.rm=T)),
             color="blue", linetype="dashed", size=1)+  theme_bw() +
  xlab("RFO_R") 
 
b <- ggplot(all_geo_res, aes(x=DeltaStd_R)) + geom_histogram(color= "#E69F00",fill="#E69F00", alpha=0.5)+ 
  geom_vline(aes(xintercept=median(DeltaStd_R,na.rm=T)),
             color="blue", linetype="dashed", size=1)+  theme_bw()+
  xlab("RFO_THR") 

c <- ggplot(all_geo_res, aes(x=DeltaStd_R,y=DeltaRank)) + geom_point(color= "#E69F00",fill="#E69F00", alpha=0.5)+ 
  theme_bw()+
  xlab("RFO_THR") +
  ylab("RFO_R") 

d <- ggplot(all_geo_res, aes(x=DeltaThr,y=DeltaRank)) + geom_point(color= "#E69F00",fill="#E69F00", alpha=0.5)+ 
  theme_bw() +
  xlab("Delta_THR") +
  ylab("RFO_R") 

e <- ggplot(all_geo_res, aes(x=richness,y=Rfinalthr)) + geom_point(color= "#E69F00",fill="#E69F00", alpha=0.5)+ 
  theme_bw()+
  xlab("richness") +
  ylab("richness_thr_after_model") 

f <- ggplot(all_geo_res, aes(x=richness,y=DeltaThr)) + geom_point(color= "#E69F00",fill="#E69F00", alpha=0.5)+ 
  theme_bw()+
  xlab("richness") +
  ylab("Delta_THR") 



set.seed(123)
pl <- list(a,c,d,b,e,f)
ml <- marrangeGrob(pl,ncol=2,nrow=3)

ggsave(file = here::here("figures/Figure7.png"),ml,width = 12, height = 12, units= "in",dpi= 300)


#'---------------------------------------------------------------------@TabeFamily

#TO DO PERCENTAGE?
dat_taxo <- dat_phylo[!is.na(dat_phylo$keep==1),]
dat_taxo <-merge(dat_taxo,data_noNA,by.x="label",by.y="row.names",all.x = T)
dat_taxo<- dat_taxo[rowSums(is.na(dat_taxo)) != ncol(dat_taxo), ]


table_taxo <- aggregate(Non_Threatened ~ Family, data = dat_taxo, sum,na.action = na.omit)
table_taxo <- table_taxo %>% arrange(desc(Non_Threatened))
grid.table()



