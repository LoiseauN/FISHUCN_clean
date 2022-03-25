# ---------- Load packages
pkgs <- c("plyr","rredlist","ggplot2","viridis","hrbrthemes","rphylopic","scales","ggalluvial","dplyr",
          "stringr","cluster","ggstatsplot","palmerpenguins","tidyverse","grid","gridExtra","raster","gridExtra")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))


#With package rredlist extract status
mammals_status <- rl_comp_groups("mammals", key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")
birds_status <- rl_comp_groups("birds", key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")
amphibians_status <- rl_comp_groups("Amphibians", key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")


#With phylopic extract pic (with License 1.0 and No Copyright)
mammals_pic <- image_data("8cad2b22-30d3-4cbd-86a3-a6d2d004b201", size = "512")[[1]]
birds_pic <- image_data("34d9872c-b7d0-416f-8ac6-1f9f952982c8", size = "512")[[1]]
fish_pic <- image_data("86c40d81-2613-4bb4-ad57-fb460be56ae5", size = "512")[[1]]
amphibians_pic <- image_data("cd0cdc36-ecfa-414f-af87-1b5e0ec0c69b", size = "512")[[1]]



#'---------------------------------------------------------------------@Comparisonothertaxa
data_4_taxa <- data.frame( taxa = c(rep("mammals", nrow(mammals_status$result)),
                                    rep("birds",nrow(birds_status$result)),
                                    rep("amphibians",nrow(amphibians_status$result)),
                                    rep("fishes",nrow(FB_final))),
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
  add_phylopic(fish_pic,      x = 4, y = 50, ysize = 8, alpha = 1)

#ggsave(file = here::here("figures/Figure1.png"),fig1,width = 12, height = 12, units= "in",dpi= 300)


#'---------------------------------------------------------------------@variable_importance

figure2 = var_imp(test_IUCN[[1]])


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

df <- data.frame(id = rep(d$species,2),
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

#ggsave(file = here::here("figures/Figure3.png"),plot_net, width = 12, height = 12, units= "in",dpi= 300)


#'---------------------------------------------------------------------@Percentagegainmodel
#'( ( valeur d'arrivée - valeur de départ ) / valeur de départ ) x 100
gainThr <- ((table(dat_network$IUCN_cat)[3]-table(dat_network$IUCN_final)[3])/table(dat_network$IUCN_cat)[3])*100
gainNThr <- ((table(dat_network$IUCN_cat)[2]-table(dat_network$IUCN_final)[2])/table(dat_network$IUCN_cat)[2])*100

#'---------------------------------------------------------------------@Protectionanalyses

MPA_Protect <- merge(MPA_Protect,dat_network,by="species")
MPA_Protect$IUCN_final <- as.factor(MPA_Protect$IUCN_final)

plt <- ggstatsplot::ggbetweenstats(
  data = MPA_Protect[c(1:100),], #data_protected,
  x = IUCN_final,
  y = log10(Target_achievement_I_IV+1),
)

plt <-  plt +
  labs(
    x = "IUCN Status",
    y = "Target achievement MPA (I - IV)"
  ) +
  scale_color_manual(values=c("#00AFBB", "#E7B800","#FC4E07"))

ggsave(file = here::here("figures/Figure4.png"),plt, width = 12, height = 12, units= "in",dpi= 300)

boxplot(MPA_Protect$Target_achievement_I_IV ~ MPA_Protect$predict_complementary, xlim = c(0, 7), ylim = c(0, 15))
boxplot(MPA_Protect$Target_achievement_I_IV ~ MPA_Protect$IUCN_cat, add = TRUE, at = 4:6)

#'---------------------------------------------------------------------@zonationanalyses
#Zrank_main$diff <- Zrank_main$rankSc2-Zrank_main$rankSc1

#fig_rank_hex <- ggplot(Zrank_main, aes(x=rankSc1, y=rankSc2) ) +
#  geom_hex(bins = 100) +
#  scale_fill_continuous(type = "viridis") +
#  theme_bw() + xlab("IUCN only")+ ylab("IUCN + Predicted")
#ggsave(file = here::here("figures/fig_rank_hex.png"),width = 12, height = 12, units= "in",dpi= 300)

fig_rank <- ggplot(all_geo_res, aes(x=rankSc1, y=rankSc2, color = log10(richness)) ) +
  geom_point(size=0.7,alpha = 0.5) + 
  scale_color_distiller(palette = "Spectral")+
  theme_bw() + xlab("IUCN")+ ylab("IUCN + Predicted") +
  geom_abline(slope=1, intercept = 0)
ggsave(file = here::here("figures/Figure5.png"),fig_rank,width = 12, height = 12, units= "in",dpi= 300)

#'---------------------------------------------------------------------@
test <- na.omit(all_geo_res)

test$pos <- NA
for (i in 1:nrow(test)){
  if(test$DeltaRank[i]>0) test$pos[i]=1
  if(test$DeltaRank[i]<0) test$pos[i]=2
}


fig_rank <- ggplot(test, aes(log10(richness), DeltaRank)) +
  geom_point(aes(fill = factor(sign(DeltaRank))), size = 3,shape=21) +
  geom_smooth(aes(colour = factor(pos)))+
  scale_fill_manual(values =c("dodgerblue2","chocolate1"))+
  scale_colour_manual(values = c("black","black"))+
    theme_bw()+
  theme(legend.position = "none")
ggsave(file = here::here("figures/Figure5bis.png"),fig_rank,width = 12, height = 12, units= "in",dpi= 300)



#'---------------------------------------------------------------------@MAP
#all_geo_res
mask.full=raster::raster(here::here("data","mask.full.tif"))


#--- Diff ranking  WHITE HOLE ARE MPA

var = c("DeltaRank","DeltaStd_R")
all_map <- lapply(1:length(var),function(x){
 
  mask = mask.full
  df <- all_geo_res[,var[x]]
  df[is.na(df)] <- 0
  mask[all_geo_res$ID] = df

  df <-as.data.frame(rasterToPoints(mask))
  colnames(df)[3] <- "value"
  
  if (var[x] =="DeltaRank" )  {  title =  "Difference Ranking IUCN/IUCN + Predict" }  
 
 
    
  if (var[x] =="DeltaRank" ){
    
  map <- ggplot() +
    geom_raster(data=df,aes(x = x, y = y, fill = value))+#(value/max(value))
    scale_fill_distiller(palette = "RdBu")+
    ggtitle(title)+
    theme_bw()+
    xlab("")+ylab("")
  


})

map <- marrangeGrob(all_map,ncol=1,nrow=2)
ggsave(file = here::here("figures/Figure6.png"),map,width = 8, height = 12, units= "in",dpi= 300)


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



