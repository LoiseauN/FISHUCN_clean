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