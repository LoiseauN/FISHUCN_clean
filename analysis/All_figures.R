#' The make.R must be run first
#' 
#' 
#' 
## Extract IUCN status ----
# mammals_status    <- rredlist::rl_comp_groups("mammals", 
#   key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")

# birds_status      <- rredlist::rl_comp_groups("birds", 
#   key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")

# amphibians_status <- rredlist::rl_comp_groups("amphibians", 
#   key ="73d6c97e1bc80791af1167c8bbd7416ac3043d28b4633c51765eff87a9cb2da3")

figure1 <- function(data){ 
all_status <- read.table(file = here::here("data","IUCN_risk.csv"), sep = ";", row.names = 1, header = T)



## Download Phylopic silhouettes (with License 1.0 and No Copyright) ----
mammals_pic    <- get_phylopic_image("8cad2b22-30d3-4cbd-86a3-a6d2d004b201", size = "512")

birds_pic      <- get_phylopic_image("34d9872c-b7d0-416f-8ac6-1f9f952982c8", size = "512")

fish_pic       <- get_phylopic_image("86c40d81-2613-4bb4-ad57-fb460be56ae5", size = "512")

amphibians_pic <- get_phylopic_image("28e237f7-9fcd-47be-8a4c-94c0ad057455", size = "512")

reptile_pic <- get_phylopic_image("f2a5ae73-c899-4e47-b0ad-b6eac3a99350", size = "512")



#'------------------------------------------------------------------------------@Comparisonothertaxa
data_4_taxa <- data.frame(taxa   = c(rep("amphibians", nrow(all_status[all_status$className == "Amphibians",])),
                                     rep("mammals", nrow(all_status[all_status$className == "Mammals",])),
                                     rep("reptiles", nrow(all_status[all_status$className == "Reptiles",])),
                                     rep("birds", nrow(all_status[all_status$className == "Birds",])),
                                     rep("marine fishes", nrow(data))),
                          
                          status = as.factor(c(all_status$rlCodes,
                                               as.character(data$IUCN))),
                          nb_sp = c(all_status$n,rep(1,nrow(data))))


levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("LR/cd", "LR/nt", "nt","NT", "LC","NThr")] <- "Non Threatened"

levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("DD",  "NA")] <- "No Status"

data_4_taxa[is.na(data_4_taxa$status),]$status <- "No Status"

levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("CR", "EN", "EW","VU","Thr")] <- "Threatened"

data_4_taxa <- subset(data_4_taxa,data_4_taxa$status != "EX")

data_4_taxa$taxa <- as.factor(data_4_taxa$taxa)

data_4_taxa <- aggregate(data_4_taxa$nb_sp, list(data_4_taxa$taxa,data_4_taxa$status), sum)
colnames(data_4_taxa) <- c("taxa", "status", "nb_sp")

data_4_taxa <- data_4_taxa[order(data_4_taxa$taxa),]
data_4_taxa$total_taxa <- c(rep(sum(data_4_taxa[data_4_taxa$taxa == "amphibians",]$n),3),
                            rep(sum(data_4_taxa[data_4_taxa$taxa == "birds",]$n),3),
                            rep(sum(data_4_taxa[data_4_taxa$taxa == "mammals",]$n),3),
                            rep(sum(data_4_taxa[data_4_taxa$taxa == "marine fishes",]$n),3),
                            rep(sum(data_4_taxa[data_4_taxa$taxa == "reptiles",]$n),3))


data_4_taxa$Freq  <- (data_4_taxa$nb_sp/data_4_taxa$total_taxa)*100 

data_4_taxa$status <- factor(data_4_taxa$status, levels = c("Threatened", "Non Threatened", "No Status"))

data_4_taxa$taxa <- factor(data_4_taxa$taxa, levels = c("birds", "reptiles", "mammals","amphibians", "marine fishes"))


fig1 <- ggplot(data_4_taxa, aes(fill=status, y=Freq, x=taxa)) + 
  geom_bar(position="stack", stat="identity",color="grey20") +
  scale_fill_manual(values = c("#FC4E07","#00AFBB", "#E7B800"), name = "IUCN status", 
                    guide = guide_legend(reverse = FALSE))+
   theme_bw() +
  xlab("")+ylab("Percentage")+
  rphylopic::add_phylopic(birds_pic,     x = 1, y = 50, ysize = 13, alpha = 1)+
  rphylopic::add_phylopic(reptile_pic,     x = 2, y = 50, ysize = 10, alpha = 1)+
  rphylopic::add_phylopic(mammals_pic,   x = 3, y = 50, ysize = 10, alpha = 1)+
  rphylopic::add_phylopic(amphibians_pic,x = 4, y = 50, ysize = 8, alpha = 1)+
  rphylopic::add_phylopic(fish_pic,      x = 5, y = 50, ysize = 7.5, alpha = 1) +
  theme( legend.position = "bottom",
         axis.text=element_text(size=18),
         axis.title=element_text(size=20),
         legend.title=element_text(size=20),
         legend.text=element_text(size=18))


grob_birds <- grobTree(textGrob(paste0("n = ", sum(data_4_taxa[data_4_taxa$taxa == "birds",]$n)), x=0.1,  y=0.95, hjust=0,
                                gp=gpar(fontsize=12,fontface="bold")))
grob_reptiles <- grobTree(textGrob(paste0("n = ", sum(data_4_taxa[data_4_taxa$taxa == "reptiles",]$n)), x=0.1,  y=0.95, hjust=0,
                                   gp=gpar(fontsize=12,fontface="bold")))
grob_mammals <- grobTree(textGrob(paste0("n = ", sum(data_4_taxa[data_4_taxa$taxa == "mammals",]$n)), x=0.1,  y=0.95, hjust=0,
                                  gp=gpar(fontsize=12,fontface="bold")))
grob_amphibians <- grobTree(textGrob(paste0("n = ", sum(data_4_taxa[data_4_taxa$taxa == "amphibians",]$n)), x=0.1,  y=0.95, hjust=0,
                                     gp=gpar(fontsize=12,fontface="bold")))
grob_fishes <- grobTree(textGrob(paste0("n = ", sum(data_4_taxa[data_4_taxa$taxa == "marine fishes",]$n)), x=0.1,  y=0.95, hjust=0,
                                 gp=gpar(fontsize=12,fontface="bold")))

# Ajouter au graphique
fig1 <- fig1 + annotation_custom(grob_birds,xmin = 0.7, xmax = 1.5, ymin = 96, ymax = 103)

fig1 <- fig1 + annotation_custom(grob_reptiles,xmin = 1.7, xmax = 2.5, ymin = 96, ymax = 103)

fig1 <- fig1 + annotation_custom(grob_mammals,xmin = 2.7, xmax = 3.5, ymin = 96, ymax = 103)

fig1 <- fig1 + annotation_custom(grob_amphibians,xmin = 3.7, xmax = 4.5, ymin = 96, ymax = 103)

fig1 <- fig1 + annotation_custom(grob_fishes,xmin = 4.7, xmax = 5.5, ymin = 96, ymax = 103)

ggsave(file = here::here("figures/Figure_1.png"),fig1,width = 12, height = 10, units= "in",dpi= 300)
}
#'---------------------------------------------------------------------@variable_importance

figure4 <- function(data =  data_noNA, model = test_IUCN[[1]]){ 
partial_plot <- var_partial(data      =  data,
                            model = model) 

# Plot importance plot
importance_plot = var_imp(model)  

fig <- importance_plot + 
  (partial_plot[[1]] /partial_plot[[3]])+ 
  (partial_plot[[2]]/partial_plot[[4]])


ggsave(file = here::here("figures/Figure_4.png"),fig,width = 12, height = 6, units= "in",dpi= 300)
}


#'---------------------------------------------------------------------@protection
figure6 <- function(data){ 

BEFORE <- cbind.data.frame(perc_cover=data$perc_cover, Target_achievement_I_IV=data$Target_achievement_I_IV,IUCN=data$IUCN_cat,What="BEFORE")
AFTER <- cbind.data.frame(perc_cover=data$perc_cover, Target_achievement_I_IV=data$Target_achievement_I_IV,IUCN=data$IUCN_final,What="AFTER")
MPA_FINAL <- rbind(BEFORE,AFTER)

MPA_FINAL$What <- factor(MPA_FINAL$What, levels = c("AFTER","BEFORE"))
MPA_FINAL$IUCN <- factor(MPA_FINAL$IUCN, levels = c("Threatened","Non Threatened","No Status"))
MPA_FINAL$category <- as.factor(paste(MPA_FINAL$IUCN, MPA_FINAL$What, sep="_"))
MPA_FINAL$category <- factor(MPA_FINAL$category,levels=c("No Status_BEFORE","No Status_AFTER","Non Threatened_BEFORE","Non Threatened_AFTER","Threatened_BEFORE","Threatened_AFTER")) 

#Target_achievement_I_IV
a <- ggplot(MPA_FINAL, aes(IUCN, Target_achievement_I_IV, fill = category)) +
  facefuns::geom_split_violin(trim = FALSE,show.legend=FALSE)+
  theme_bw()+
  coord_cartesian(ylim = c(0.1, 500))+
  scale_y_continuous(
    trans  = "log10",
    breaks = c(0.1,1, 10, 100)) +
  scale_fill_manual(values = c("#fee6b7", "#e7b800","#c1e4e8", "#00AFBB",  "#FEDBCD" , "#FC4E07" ))+
  theme(legend.position = "none") +
  geom_boxplot(width = 0.25, notch = FALSE, outlier.shape = NA, coef=0, lwd=0.7)+
  ylab("Target achievement (%)")

#perc_cover
b <- ggplot(MPA_FINAL, aes(IUCN, perc_cover, fill = category)) +
  facefuns::geom_split_violin(trim = FALSE,show.legend=FALSE)+
  theme_bw()+
  coord_cartesian(ylim = c(0.1, 100))+
  scale_y_continuous(
    trans  = "log10",
    breaks = c(0.1,1, 10,100)) +
  scale_fill_manual(values = c("#fee6b7", "#e7b800","#c1e4e8", "#00AFBB",  "#FEDBCD" , "#FC4E07" ))+
  theme(legend.position = "none") +
  geom_boxplot(width = 0.25, notch = FALSE, outlier.shape = NA, coef=0, lwd=0.7)+
  ylab("Cover (%)")

fig <- gridExtra::grid.arrange(a,b,ncol=2)

ggsave(file = here::here("figures/Figure_6.png"),fig,width = 12, height = 6, units= "in",dpi= 300)

}



