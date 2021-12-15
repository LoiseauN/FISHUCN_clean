# ---------- Load packages
pkgs <- c("plyr","rredlist","ggplot2","viridis","hrbrthemes","rphylopic","scales","ggalluvial","dplyr",
          "stringr","cluster","ggstatsplot","palmerpenguins","tidyverse","grid","gridExtra","raster","gridExtra")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

# ---------- Load data
load(file=file.path("outputs","all_predict.RData"))

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
  scale_fill_manual(values = c("firebrick1", "forestgreen", "grey35"), name = "IUCN status", 
                    guide = guide_legend(reverse = TRUE))+
  theme_bw() +
  xlab("Taxa")+ylab("Percentage")+
  add_phylopic(birds_pic,     x = 1, y = 50, ysize = 12, alpha = 1)+
  add_phylopic(mammals_pic,   x = 2, y = 50, ysize = 10, alpha = 1)+
  add_phylopic(amphibians_pic,x = 3, y = 50, ysize = 10, alpha = 1)+
  add_phylopic(fish_pic,      x = 4, y = 50, ysize = 8, alpha = 1)

ggsave(file = here::here("figures/fig1.png"),width = 12, height = 12, units= "in",dpi= 300)

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

dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c(NA,"NaN"), 
                                  to = c("No Status","No Status")))


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
  scale_fill_manual(values = c("firebrick1", "forestgreen", "grey35","pink"), name = "IUCN status", 
                    guide = guide_legend(reverse = TRUE)) +
  geom_stratum(alpha = 1,color="white") +
  geom_text(stat = "stratum",
            aes(label = percent(after_stat(prop), accuracy = .1)))+
  theme_bw()+
  xlab("") +ylab("Number of species")

ggsave(file = here::here("figures/fig2.png"),width = 12, height = 12, units= "in",dpi= 300)


#'---------------------------------------------------------------------@Percentagegainmodel
#'
#'
#'( ( valeur d'arrivée - valeur de départ ) / valeur de départ ) x 100
#'---------------------------------------------------------------------@Protectionanalyses
load("PctMPAFish.RData")
PctMPAFish$species <- str_replace(PctMPAFish$species, "_", "-")

data_protected <- dat_network
PctMPAFish[is.na(PctMPAFish)] <- 0

data_protected <- merge(data_protected,PctMPAFish,by="row.names",all.x = T)
data_protected <- data_protected[,-c(1:2)]
colnames(data_protected)[4]<- "species"
data_protected$IUCN_final <- as.factor(data_protected$IUCN_final)

plt <- ggstatsplot::ggbetweenstats(
  data = data_protected,
  x = IUCN_final,
  y = AreaMPAI_IV,
 )

plt +
  labs(
    x = "IUCN Status",
    y = "% cover MPA (I - IV)"
    ) +
  scale_color_manual(values=c("grey35", "forestgreen","firebrick1"))


#'---------------------------------------------------------------------@zonationanalyses
Zrank_main$diff <- Zrank_main$rankSc2-Zrank_main$rankSc1

fig_rank_hex <- ggplot(Zrank_main, aes(x=rankSc1, y=rankSc2) ) +
  geom_hex(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() + xlab("IUCN only")+ ylab("IUCN + Predicted")
ggsave(file = here::here("figures/fig_rank_hex.png"),width = 12, height = 12, units= "in",dpi= 300)

fig_rank <- ggplot(Zrank_main, aes(x=rankSc1, y=rankSc2, color = diff) ) +
  geom_point(size=0.7) + 
  scale_color_continuous(type = "viridis",direction = -1) +
  theme_bw() + xlab("IUCN")+ ylab("IUCN + Predicted") +
  geom_abline(slope=1, intercept = 0)
ggsave(file = here::here("figures/fig_rank.png"),width = 12, height = 12, units= "in",dpi= 300)


# MAP

mask.full=raster(here::here("data","mask.full.tif"))

maskdiff = mask.full
maskdiff[Zrank_main$ID] = Zrank_main$diff
maskSc1 = mask.full
maskSc1[Zrank_main$ID] = Zrank_main$rankSc1
maskSc2 = mask.full
maskSc2[Zrank_main$ID] = Zrank_main$rankSc2


RankDiff <-as.data.frame(rasterToPoints(maskdiff))
colnames(RankDiff)[3] = "Diff"
DIFF_MAP <- ggplot() +
  geom_tile(data=RankDiff,aes(x = x, y = y, fill = Diff))+
  scale_fill_gradient2(low = "blue", midpoint = 0, mid = "yellow", high = "red", name = "Diffence") +
  ggtitle("Difference Ranking IUCN/IUCN + Predict")+
  theme_bw()+
  xlab("")+ylab("")


RankSc1 <-as.data.frame(rasterToPoints(maskSc1))
colnames(RankSc1)[3] = "Rank"
IUCN_MAP <- ggplot() +
  geom_tile(data=RankSc1,aes(x = x, y = y, fill = Rank))+
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = median(Zrank_main$rankSc1))+
  ggtitle("IUCN")+
  theme_bw()+
  xlab("")+ylab("")

RankSc2<-as.data.frame(rasterToPoints(maskSc2))
colnames(RankSc2)[3] = "Rank"
IUCN_MAP_predict <- ggplot() +
  geom_tile(data=RankSc2,aes(x = x, y = y, fill = Rank))+
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = median(Zrank_main$rankSc2))+
  ggtitle("IUCN + Predict")+
  theme_bw()+
  xlab("")+ylab("")

map <- grid.arrange(DIFF_MAP,IUCN_MAP,IUCN_MAP_predict,ncol =1)
ggsave(file = here::here("figures/map_rank.png"),width = 15.75, height = 24.00, units= "in",dpi= 300)

#Data for zonation
#datazonation <- dat_network[,c("species","IUCN_status","IUCN_final")]
#datazonation$species <- gsub("-", "_", datazonation$species)
#save(datazonation,file="datazonation.RData")

#TEST 5% 
RankScTEST2<-as.data.frame(rasterToPoints(maskSc2))
colnames(RankScTEST2)[3] = "Rank"
RankScTEST2<- RankScTEST2[order(RankScTEST2$Rank,decreasing = T),]
RankScTEST2$Rank[c(150000:646384)] <- 0



IUCN_MAP <- ggplot() +
  geom_tile(data=RankScTEST2,aes(x = x, y = y, fill = Rank))+
  scale_fill_gradient2(low = "blue", high = "red")+
  ggtitle("IUCN")+
  theme_bw()+
  xlab("")+ylab("")


RankScTEST3<-as.data.frame(rasterToPoints(maskSc3))
colnames(RankScTEST3)[3] = "Rank"
RankScTEST3<- RankScTEST3[order(RankScTEST3$Rank,decreasing = T),]
RankScTEST3$Rank[c(150000:646384)] <- 0


IUCN_MAP_predict <- ggplot() +
  geom_tile(data=RankScTEST3,aes(x = x, y = y, fill = Rank))+
  scale_fill_gradient2(low = "blue", high = "red")+
  ggtitle("IUCN + Predict")+
  theme_bw()+
  xlab("")+ylab("")

grid.arrange(IUCN_MAP,IUCN_MAP_predict,ncol =1)



#Supplementary 
# Load library
library(VennDiagram)

# Generate 3 sets of 200 words
set1 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set2 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set3 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set4 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
# Chart
venn.diagram(
  x = list(set1, set2, set3,set4),
  category.names = c("Set 1" , "Set 2 " , "Set 3", "Set 4"),
  filename = '#14_venn_diagramm.png',
  output=TRUE
)

venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1" , "Set 2 " , "Set 3"),
  filename = NULL,
  output=TRUE
)


