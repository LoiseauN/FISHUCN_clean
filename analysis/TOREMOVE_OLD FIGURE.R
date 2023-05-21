#'---------------------------------------------------------------------@zonationanalyses
all_geo_res <- all_geo_res[order(all_geo_res$richness, decreasing=FALSE), ]
all_geo_res <- na.omit(all_geo_res)


fig_rank <- ggplot(all_geo_res, aes(x=rankSc1, y=rankSc2, color = log10(richness))) +
  geom_point(size=2.5,alpha = 0.3,shape=16) + 
  #scale_color_distiller(palette = "Spectral")+
  #scale_color_gradient(low = "#00AFBB", high = "#FC4E07")+
  scale_color_hp(option = "Gryffindor")+
  theme_bw() + xlab("Cell rank BEFORE")+ ylab("Cell rank AFTER") +
  geom_abline(slope=1, intercept = 0)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.position = c(0.80, 0.08),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.direction = "horizontal")
ggsave(file = here::here("figures/Figure6a.png"),fig_rank,width = 12, height = 12, units= "in",dpi= 300)







#pal <- wes_palette("Zissou1", max(c(all_geo_res$Rnothr,all_geo_res$Rfinalnothr),na.rm=T), type = "continuous")

all_geo_res$Perthrbefore <- all_geo_res$Rthr/(all_geo_res$Rthr+all_geo_res$Rnothr+all_geo_res$Rnostatus)
all_geo_res$Perthrfinal <- all_geo_res$Rfinalthr/(all_geo_res$Rfinalthr+all_geo_res$Rfinalnothr+all_geo_res$Rfinalnostatus)

all_geo_res$Pernothrbefore <- all_geo_res$Rnothr/(all_geo_res$Rthr+all_geo_res$Rnothr+all_geo_res$Rnostatus)
all_geo_res$Pernothrfinal <- all_geo_res$Rfinalnothr/(all_geo_res$Rfinalthr+all_geo_res$Rfinalnothr+all_geo_res$Rfinalnostatus)

all_geo_res$Pernostatusbefore <- all_geo_res$Rnostatus/(all_geo_res$Rthr+all_geo_res$Rnothr+all_geo_res$Rnostatus)
all_geo_res$Pernostatusfinal <- all_geo_res$Rfinalnostatus/(all_geo_res$Rfinalthr+all_geo_res$Rfinalnothr+all_geo_res$Rfinalnostatus)



#scale_alpha(range=c(0.5,0.5))+
#scale_fill_manual(name = "mask.full", values = my_colors) +
#scale_fill_distiller(palette='RdYlBu',limits = c(min(c(all_geo_res$Rthr,all_geo_res$Rfinalthr)), 
#                                                max(c(all_geo_res$Rthr,all_geo_res$Rfinalthr)))) + 
#scale_color_distiller(palette='RdYlBu',limits = c(min(c(all_geo_res$Rthr,all_geo_res$Rfinalthr)), 
#                                                  max(c(all_geo_res$Rthr,all_geo_res$Rfinalthr)))) + 


#'--------------------------------------------------------@PerTHRbefore
if (var[x] =="Perthrbefore" )  {  title =  "Percentage THR IUCN" 
map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full)) +
  scale_fill_gradient(low="#00AFBB",high="#FC4E07", space ="Lab" , 
                      limits = c(min(c(all_geo_res$PerTHRbefore,all_geo_res$PerTHRfinal),na.rm=T), 
                                 max(c(all_geo_res$PerTHRbefore,all_geo_res$PerTHRfinal),na.rm=T)))+
  scale_colour_gradient(low="#00AFBB",high="#FC4E07", space ="Lab" , 
                        limits = c(min(c(all_geo_res$PerTHRbefore,all_geo_res$PerTHRfinal),na.rm=T), 
                                   max(c(all_geo_res$PerTHRbefore,all_geo_res$PerTHRfinal),na.rm=T)))+
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+
#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/PerTHRbefore.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

#'--------------------------------------------------------@DeltaThreatened

#min(mask.full.polygon$mask.full[log10(mask.full.polygon$mask.full+1)>0])


if  (var[x] =="DeltaThr" )  {  title =  "Delta Richness Threatened"  
map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full)) +
  #viridis::scale_fill_viridis(option = "inferno")+
  scale_fill_gradient2(low = "white",mid="#00AFBB", midpoint = 0,
                       high="#FC4E07", space ="Lab" )+
  scale_colour_gradient2(low = "white",mid="#00AFBB", midpoint = 0,
                         high="#FC4E07", space ="Lab" )+
  
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+
#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/DeltaRichness.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}


#'--------------------------------------------------------@RANKBEFORE

if  (var[x] =="rankSc1" )  {  title =  "BEFORE Rank"  
pal <- wes_palette("Zissou1",  max(c(all_geo_res$rankSc1,all_geo_res$rankSc2),na.rm=T), type = "continuous")

map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full)) +
  scale_fill_gradientn(colours = pal, na.value = "#458B00B3",
                       limits = c(min(c(all_geo_res$rankSc1,all_geo_res$rankSc2),na.rm=T), 
                                  max(c(all_geo_res$rankSc1,all_geo_res$rankSc2),na.rm=T)))+
  scale_colour_gradientn(colours = pal, na.value = "#458B00B3",
                         limits = c(min(c(all_geo_res$rankSc1,all_geo_res$rankSc2),na.rm=T), 
                                    max(c(all_geo_res$rankSc1,all_geo_res$rankSc2),na.rm=T)))+
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+

ggsave(file = here::here("figures/RankBefore.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}



#'--------------------------------------------------------@RANKAFTER

if  (var[x] =="rankSc2" )  {  title =  "AFTER Rank"  
pal <- wes_palette("Zissou1",  max(c(all_geo_res$rankSc1,all_geo_res$rankSc2),na.rm=T), type = "continuous")

map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full)) +
  scale_fill_gradientn(colours = pal, na.value = "#458B00B3",
                       limits = c(min(c(all_geo_res$rankSc1,all_geo_res$rankSc2),na.rm=T), 
                                  max(c(all_geo_res$rankSc1,all_geo_res$rankSc2),na.rm=T)))+
  scale_colour_gradientn(colours = pal, na.value = "#458B00B3",
                         limits = c(min(c(all_geo_res$rankSc1,all_geo_res$rankSc2),na.rm=T), 
                                    max(c(all_geo_res$rankSc1,all_geo_res$rankSc2),na.rm=T)))+
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+

ggsave(file = here::here("figures/RankAfter.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}
#'--------------------------------------------------------@PerTHRfinal
if (var[x] =="Perthrfinal" )  {  title =  "Percentage IUCN + predicted THR " 
map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full)) +
  scale_fill_gradient(low="#00AFBB",high="#FC4E07", space ="Lab" , 
                      limits = c(min(c(all_geo_res$PerTHRbefore,all_geo_res$PerTHRfinal),na.rm=T), 
                                 max(c(all_geo_res$PerTHRbefore,all_geo_res$PerTHRfinal),na.rm=T)))+
  scale_colour_gradient(low="#00AFBB",high="#FC4E07", space ="Lab" , 
                        limits = c(min(c(all_geo_res$PerTHRbefore,all_geo_res$PerTHRfinal),na.rm=T), 
                                   max(c(all_geo_res$PerTHRbefore,all_geo_res$PerTHRfinal),na.rm=T)))+
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12)) #+
#guides(fill = guide_legend(nrow = 1))
ggsave(file = here::here("figures/PerTHRfinal.png"),map,width = 12, height = 8, units= "in",dpi= 300)
rm(map)
}

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


fig_rank <- ggplot(all_geo_res, aes(x=richness, y=DeltaThr, color = log10(richness))) +
  geom_point(size=2.5,alpha = 0.3,shape=16) + 
  scale_color_distiller(palette = "Spectral")+
  theme_bw() + xlab("Cell rank BEFORE")+ ylab("Cell rank AFTER") 
ggsave(file = here::here("figures/test.png"),fig_rank,width = 12, height = 12, units= "in",dpi= 300)

fig_rank <- ggplot(all_geo_res, aes(x=Rnothr, y=Rfinalnothr, color = log10(richness))) +
  geom_point(size=2.5,alpha = 0.3,shape=16) + 
  scale_color_distiller(palette = "Spectral")+
  theme_bw() + xlab("Cell rank BEFORE")+ ylab("Cell rank AFTER") 
ggsave(file = here::here("figures/test2.png"),fig_rank,width = 12, height = 12, units= "in",dpi= 300)



#'---------------------------------------------------------------------@MAP
#all_geo_res
mask.full=raster::raster(here::here("data","mask.full.tif"))
mask.full.polygon <- sf::st_as_sf(as.point = F)

#--- Diff ranking  WHITE HOLE ARE MPA

var = c("DeltaRank")
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
  
  title =  "Difference Ranking IUCN/IUCN + Predict"
  
  map <- ggplot(data = mask.full.polygon) +
    geom_sf(aes(fill = mask.full), color = NA)+#(value/max(value))
    scale_fill_distiller(palette = "RdBu")+
    ggtitle(title)+
    theme_bw()+
    xlab("")+ylab("")
  
  ggsave(file = here::here("figures/map_test.png"),map,width = 8, height = 12, units= "in",dpi= 300)
  
  
  
  # map <- ggplot() +
  #  geom_raster(data=df,aes(x = x, y = y, fill = value))+#(value/max(value))
  # scale_fill_distiller(palette = "RdBu")+
  #  ggtitle(title)+
  # theme_bw()+
  #  xlab("")+ylab("")
  
  
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







#My data
mask.full=raster::raster(here::here("data","mask.full.tif"))

mask = mask.full
df <- all_geo_res[,var[x]]
df[is.na(df)] <- 0
mask[all_geo_res$ID] = df

#raster to stars
mask <- stars::st_as_stars(mask)
mask.full.polygon <- sf::st_as_sf(mask,as.point = F)
mask.full.polygon <-  fortify(mask.full.polygon)

mask.full.polygon <- sf::st_transform(mask.full.polygon, crs=mol)


map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = scale(mask.full), color = scale(mask.full))) +
  #viridis::scale_fill_viridis(option = "inferno")+
  scale_fill_gradient2(midpoint= 0, low="#00AFBB", mid="white",
                       high="#FC4E07", space ="Lab" )+
  scale_color_gradient2(midpoint= 0, low="#00AFBB", mid="white",
                        high="#FC4E07", space ="Lab" )+
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  #scale_fill_hp(option = "Ravenclaw", 
  #              limits = c(min(all_geo_res$DeltaRank),na.rm=T),
  #                         max(all_geo_res$DeltaRank,na.rm=T))+
  geom_sf(data = world, fill = "black", color = "black", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  geom_mapframe(mol, colour = "black", size = 0.4) +
  
  ggtitle(title) +
  
  ggthemes::theme_map(base_family = "serif") +
  theme(legend.position = "bottom", 
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18),
        legend.text     = element_text(face = "plain", size = 12))












map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
  scale_fill_distiller(palette='RdYlBu') + 
  scale_color_distiller(palette='RdYlBu',) + 
  scale_alpha(range=c(0.5,0.5))+
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  #scale_fill_hp(option = "Ravenclaw", 
  #              limits = c(min(all_geo_res$DeltaRank),na.rm=T),
  #                         max(all_geo_res$DeltaRank,na.rm=T))+
  geom_sf(data = world, fill = "#bebebe", color = "white", size = 0.1) +
  geom_graticules(mol) +
  geom_mapframe(mol, colour = "white", size = 2.0) +
  #geom_mapframe(mol, colour = "black", size = 0.4) +
  
  
  ylab(" ") +
  xlab(" ") +
  
  #ggthemes::theme_map(base_family = "serif") +
  theme_bw()+
  theme(legend.position = c(0.85, 0.1),
        legend.direction = "horizontal",
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18, hjust = 1),
        legend.text     = element_text(face = "plain", size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
        # legend.key.height= unit(1.5, 'cm'),
        #legend.key.width= unit(2, 'cm')
        #axis.text=element_text(size=14),
        #axis.title=element_text(size=14,face="bold")
  ) 
ggsave(file = here::here("figures/testMap.png"),map,width = 12, height = 8, units= "in",dpi= 300)




map <- ggplot(world) +
  geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
  harrypotter::scale_fill_hp(house = "ravenclaw",na.value = "transparent",name = " ") +
  harrypotter::scale_color_hp(house = "ravenclaw",na.value = "transparent",name = " ") +
  scale_alpha(range=c(0.5,0.5))+
  #scale_fill_manual(name = "mask.full", values = my_colors) +
  #scale_fill_hp(option = "Ravenclaw", 
  #              limits = c(min(all_geo_res$DeltaRank),na.rm=T),
  #                         max(all_geo_res$DeltaRank,na.rm=T))+
  geom_sf(data = world, fill = "white" ,#"#bebebe", 
          color = "black",#"white" 
          size = 0.1) +
  geom_graticules(mol) +
  #geom_mapframe(mol, colour = "white", size = 2.0) +
  #geom_mapframe(mol, colour = "black", size = 0.4) +
  
  
  ylab(" ") +
  xlab(" ") +
  
  #ggthemes::theme_map(base_family = "serif") +
  cowplot::theme_minimal_grid()   +
  theme(legend.position = c(0.85, 0.1),
        legend.direction = "horizontal",
        legend.title    = element_blank(), 
        plot.title      = element_text(face = "bold",  size = 18, hjust = 1),
        legend.text     = element_text(face = "plain", size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
        # legend.key.height= unit(1.5, 'cm'),
        #legend.key.width= unit(2, 'cm')
        #axis.text=element_text(size=14),
        #axis.title=element_text(size=14,face="bold")
  ) 











CellsIn = exactextractr::exact_extract(mask,mollBorder,include_xy=TRUE, include_cell = TRUE)

dim(CellsIn[[1]])
all_geo_res <- all_geo_res2

all_geo_res <- merge(all_geo_res,CellsIn, by.x = "ID",by.y = "cell" )



# NEED TO COMPUTE A PERCENTAGE OU TAUX DE VARIATION VERS LE HAUT OU LE BAS

fig_rank <- ggplot(all_geo_res, aes(x=rankSc1, y=rankSc2, color = log10(richness))) +
  geom_point(size=2.5,alpha = 0.3,shape=16) +
  #scale_color_distiller(palette = "Spectral")+
  scale_color_gradient(low = "#00AFBB", high = "#FC4E07")+
  theme_bw() + xlab("Cell rank BEFORE")+ ylab("Cell rank AFTER") +
  geom_abline(slope=1, intercept = 0)
ggsave(file = here::here("figures/Figure5.png"),fig_rank,width = 12, height = 12, units= "in",dpi= 300)

all_geo_res2$richness <- all_geo_res2$Rthr+all_geo_res2$Rnothr+all_geo_res2$Rnostatus

all_geo_res2 <- na.omit(all_geo_res2)
all_geo_res  <- all_geo_res2[sample(nrow(all_geo_res2), 1000), ]

all_geo_res$ScalerankSc1<- scale(all_geo_res$rankSc1)
all_geo_res$ScalerankSc2<- scale(all_geo_res$rankSc2)
all_geo_res$Scalerichness<- scale(all_geo_res$richness)

all_geo_res$taux <- ((all_geo_res$rankSc1-all_geo_res$rankSc2)/all_geo_res$rankSc1)*100

plot(all_geo_res$richness, # x-axis
     all_geo_res$rankSc1-all_geo_res$rankSc2, # y axis
     type="n",yaxt="n",ylim=c(min(all_geo_res$rankSc1),max(all_geo_res$rankSc1)),
     xlim=c(min(all_geo_res$richness),max(all_geo_res$richness)), # empty plot, sets axis limits.
     xaxt="n",ylab="",lty=1,
     main="Change in prioritization",
     pch=16,cex.main=1.4)

mtext(side=2,
      "Change in ranking"
      ,cex=.8,line=1) # y axis title
abline(v=0, col="lightgray") # gridlines
abline(v=0.2, col="lightgray")
abline(v=0.4, col="lightgray")
abline(v=0.6, col="lightgray")
abline(v=0.8, col="lightgray")
abline(v=1, col="lightgray")
## Note: the next 3 lines of code are designed for a
## specific plot size. They will need to be adjusted if resized.
text(x=1.05,y=0.007,"Delta",col="darkgray",cex=1.1)
text(x=1.05,y=-.007,"Rank",col="darkgray",cex=1.1)

for (i in 1:nrow(all_geo_res)){
  segments(x0=all_geo_res$richness[i],
           x1=all_geo_res$richness[i],
           y0=all_geo_res$rankSc1[i],
           y1=all_geo_res$rankSc2[i],
           col=wes_palette(name="Zissou1")[1],lwd=2)
}


abline(h=0,col="darkgray") # x axis = 0
points(all_geo_res$richness,
       all_geo_res$rankSc1,
       yaxt="n",ylim=c(0,1),xlim=c(0,1),
       xaxt="n",lty=1,bg="darkgray",
       pch=23,cex=.9)
points(all_geo_res$richness,
       all_geo_res$rankSc2,
       yaxt="n",ylim=c(0,1),xlim=c(0,1),
       xaxt="n",lty=1,pch=21,
       bg="black",cex=.9)

axis(2,at=c(seq(-.3,.3,by=.1)),
     labels=c("-30%","-20%","-10%","0%","10%","20%","30%"),
     cex.axis=1,las=1)
axis(1,at=c(seq(0,1,by=.1)),labels=c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%"),tick=TRUE,cex.axis=.8)
axis(1,at=c(seq(0,1,by=.1)),
     labels=FALSE,tick=FALSE,cex.axis=1)
#legend(x=.8,y=-.15,c("Men","Women"," "," "),pt.bg=c("darkgray",wes_palette(name="Rushmore")[1],wes_palette(name="Rushmore")[3],wes_palette(name="Rushmore")[5]),pch=c(23,21,21,21),cex=.9,y.intersp=.5,x.intersp=1,bty="n")
legend(x=.82,y=.3,c("Men","Women"),
       pt.bg=c("darkgray","black"),
       pch=c(23,21),cex=1.1,y.intersp=.6,
       x.intersp=1,bty="n")

dev.off()








#'---------------------------------------------------------------------@ResultsPrediction
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






#Zrank_main$diff <- Zrank_main$rankSc2-Zrank_main$rankSc1

#fig_rank_hex <- ggplot(Zrank_main, aes(x=rankSc1, y=rankSc2) ) +
#  geom_hex(bins = 100) +
#  scale_fill_continuous(type = "viridis") +
#  theme_bw() + xlab("IUCN only")+ ylab("IUCN + Predicted")
#ggsave(file = here::here("figures/fig_rank_hex.png"),width = 12, height = 12, units= "in",dpi= 300)
#all_geo_res <- all_geo_res[,-c(10,11)]
#all_geo_res <- merge(all_geo_res,Zrank_main,by = "ID",all.x=T)
#all_geo_res$DeltaRank <- all_geo_res$rankSc2-all_geo_res$rankSc1






##--------------------SUPPLEMENTARY
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



