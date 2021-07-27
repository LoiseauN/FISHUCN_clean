# ---------- Load packages
pkgs <- c("plyr","rredlist","ggplot2","viridis","hrbrthemes","rphylopic","scales","ggalluvial")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

# ---------- Load data
load(file=file.path(results_dir,"preds_final.RData"))

#With package rredlist extract status
mammals_status <- rl_comp_groups("mammals", key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")
birds_status <- rl_comp_groups("birds", key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")
amphibians_status <- rl_comp_groups("Amphibians", key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")


#With phylopic extract pic (with License 1.0 and No Copyright)
mammals_pic <- image_data("8cad2b22-30d3-4cbd-86a3-a6d2d004b201", size = "512")[[1]]
birds_pic <- image_data("34d9872c-b7d0-416f-8ac6-1f9f952982c8", size = "512")[[1]]
fish_pic <- image_data("86c40d81-2613-4bb4-ad57-fb460be56ae5", size = "512")[[1]]
amphibians_pic <- image_data("cd0cdc36-ecfa-414f-af87-1b5e0ec0c69b", size = "512")[[1]]



############################# Figure 1 #########################################
data_4_taxa <- data.frame( taxa = c(rep("mammals", nrow(mammals_status$result)),
                                    rep("birds",nrow(birds_status$result)),
                                    rep("amphibians",nrow(amphibians_status$result)),
                                    rep("fishes",nrow(FB_vars))),
                           status = as.factor(c(mammals_status$result$category,
                                                birds_status$result$category,
                                                amphibians_status$result$category,
                                                as.character(FB_vars$IUCN))))

levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("LR/cd", "LR/nt", "nt","NT", "LC")] <- "Non Threatened"

levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("DD",  "NA")] <- "No Status"

data_4_taxa[is.na(data_4_taxa$status),]$status <- "No Status"

levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("CR", "EN", "EW","VU")] <- "Threatened"

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

ggplot(data_4_taxa, aes(fill=status, y=Freq, x=taxa)) + 
  geom_bar(position="stack", stat="identity",color="grey20") +
  scale_fill_manual(values = c("firebrick1", "forestgreen", "grey35"), name = "IUCN status", 
                    guide = guide_legend(reverse = TRUE))+
  theme_bw() +
  xlab("Taxa")+ylab("Percentage")+
  add_phylopic(birds_pic,     x = 1, y = 50, ysize = 12, alpha = 1)+
  add_phylopic(mammals_pic,   x = 2, y = 50, ysize = 10, alpha = 1)+
  add_phylopic(amphibians_pic,x = 3, y = 50, ysize = 10, alpha = 1)+
  add_phylopic(fish_pic,      x = 4, y = 50, ysize = 8, alpha = 1)


############################# Figure Results #########################################

#Network

data_final_zonation


dat_network <- data.frame(data_final_zonation$main[,c("species","IUCN_alone","predict")])

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

not_in_model <- data.frame(species = rownames(FB_vars[!rownames(FB_vars)%in% dat_network$species,]),
           IUCN_alone= rep("<NA>", length(rownames(FB_vars[!rownames(FB_vars)%in% dat_network$species,]))),
             predict=rep("<NA>", length(rownames(FB_vars[!rownames(FB_vars)%in% dat_network$species,]))))



dat_network <- rbind(dat_network,not_in_model)
#dat_network<-as.data.frame(sapply(dat_network,
 #                                 mapvalues, from = c("LC","NT","nt","NC"), 
  #                                to = c("Non Threatened","Non Threatened",
   #                                      "Non Threatened","Non Threatened")))





dat_network$IUCN_final <- NA

for (i in 1:nrow (dat_network)){
  if(is.na(dat_network$IUCN_alone[i])){ dat_network$IUCN_final[i]=dat_network$predict[i]
  
  }else{
    
    dat_network$IUCN_final[i]=dat_network$IUCN_alone[i]
    
  }
}

dat_network<-as.data.frame(sapply(dat_network,
                                  mapvalues, from = c(NA), 
                                  to = c("No Status")))



df <- data.frame('id' = rep(dat_network$species,2),
                 'stage' = as.factor(c(rep("Before Prediction",nrow(dat_network)), rep("After Prediction",nrow(dat_network)))),
                 'group' = as.factor(c(dat_network$IUCN_alone,dat_network$IUCN_final)))
df <- transform(df,
                group = factor(group, rev(levels(group))))
df <- transform(df,
                stage = factor(stage, rev(levels(stage))))

plot_net <- 
  ggplot(df, aes(x = stage, stratum = group, alluvium = id, fill = group, label = stage)) +
  scale_x_discrete(expand = c(.15, .15)) +
  geom_flow(color="white") +
  scale_fill_manual(values = c("firebrick1", "forestgreen", "grey35"), name = "IUCN status", 
                    guide = guide_legend(reverse = TRUE))+
  geom_stratum(alpha = 1,color="white") +
  geom_text(stat = "stratum",
            aes(label = percent(after_stat(prop), accuracy = .1)))+
  theme_bw()+
  xlab("") +ylab("Number of species")

plot_net



data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           weight = freq,
           fill = response, label = response)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses at three points in time")


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


# MAP
rasterFromXYZ()

#Data for zonation
datazonation <- dat_network[,c("species","IUCN_status","IUCN_final")]
datazonation$species <- gsub("-", "_", datazonation$species)
save(datazonation,file="datazonation.RData")
