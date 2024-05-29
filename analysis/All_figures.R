#' The make.R must be run first
#' 
#' 
#' 
## Extract IUCN status ----


figure1 <- function(data){ 
all_status <- read.table(file = here::here("data","IUCN_risk.csv"), sep = ";", row.names = 1, header = T)
all_status <- all_status[rownames(all_status) %in% c("AMPHIBIA","AVES","MAMMALIA","REPTILIA"),]
rownames(all_status) <- tolower(rownames(all_status)) 
all_status <- as.matrix(all_status)
all_status <- as.data.frame(segregation::matrix_to_long(all_status, group = "rlCodes", unit = "className"))



## Download Phylopic silhouettes (with License 1.0 and No Copyright) ----
mammals_pic    <- get_phylopic_image("8cad2b22-30d3-4cbd-86a3-a6d2d004b201", size = "512")

aves_pic      <- get_phylopic_image("34d9872c-b7d0-416f-8ac6-1f9f952982c8", size = "512")

fish_pic       <- get_phylopic_image("86c40d81-2613-4bb4-ad57-fb460be56ae5", size = "512")

amphibians_pic <- get_phylopic_image("28e237f7-9fcd-47be-8a4c-94c0ad057455", size = "512")

reptile_pic <- get_phylopic_image("f2a5ae73-c899-4e47-b0ad-b6eac3a99350", size = "512")



#'------------------------------------------------------------------------------@Comparisonothertaxa
data_4_taxa <- data.frame(taxa   = c(rep("amphibia", nrow(all_status[all_status$className == "amphibia",])),
                                     rep("mammalia", nrow(all_status[all_status$className == "mammalia",])),
                                     rep("reptilia", nrow(all_status[all_status$className == "reptilia",])),
                                     rep("aves", nrow(all_status[all_status$className == "aves",])),
                                     rep("marine fishes", nrow(data))),
                          
                          status = as.factor(c(all_status$rlCodes,
                                               as.character(data$IUCN))),
                          nb_sp = c(all_status$n,rep(1,nrow(data))))


levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("LR.cd","NT.or.LR.nt","LC.or.LR.lc","NThr")] <- "Non Threatened"

levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("DD","NA")] <- "No Status"

data_4_taxa[is.na(data_4_taxa$status),]$status <- "No Status"

levels(data_4_taxa$status)[levels(data_4_taxa$status) %in% c("CR.PE.","CR.PEW.","CR","EN","VU", "Thr")] <- "Threatened"

data_4_taxa <- subset(data_4_taxa,data_4_taxa$status != "EX")

data_4_taxa$taxa <- as.factor(data_4_taxa$taxa)

data_4_taxa <- aggregate(data_4_taxa$nb_sp, list(data_4_taxa$taxa,data_4_taxa$status), sum)
colnames(data_4_taxa) <- c("taxa", "status", "nb_sp")

data_4_taxa <- data_4_taxa[order(data_4_taxa$taxa),]
data_4_taxa$total_taxa <- c(rep(sum(data_4_taxa[data_4_taxa$taxa == "amphibians",]$n),3),
                            rep(sum(data_4_taxa[data_4_taxa$taxa == "aves",]$n),3),
                            rep(sum(data_4_taxa[data_4_taxa$taxa == "mammals",]$n),3),
                            rep(sum(data_4_taxa[data_4_taxa$taxa == "marine fishes",]$n),3),
                            rep(sum(data_4_taxa[data_4_taxa$taxa == "reptiles",]$n),3))


data_4_taxa$Freq  <- (data_4_taxa$nb_sp/data_4_taxa$total_taxa)*100 

data_4_taxa$status <- factor(data_4_taxa$status, levels = c("Threatened", "Non Threatened", "No Status"))

data_4_taxa$taxa <- factor(data_4_taxa$taxa, levels = c("aves", "reptiles", "mammals","amphibians", "marine fishes"))


fig1 <- ggplot(data_4_taxa, aes(fill=status, y=Freq, x=taxa)) + 
  geom_bar(position="stack", stat="identity",color="grey20") +
  scale_fill_manual(values = c("#FC4E07","#00AFBB", "#E7B800"), name = "IUCN status", 
                    guide = guide_legend(reverse = FALSE))+
   theme_bw() +
  xlab("")+ylab("Percentage")+
  rphylopic::add_phylopic(aves_pic,     x = 1, y = 50, ysize = 13, alpha = 1)+
  rphylopic::add_phylopic(reptile_pic,     x = 2, y = 50, ysize = 10, alpha = 1)+
  rphylopic::add_phylopic(mammals_pic,   x = 3, y = 50, ysize = 10, alpha = 1)+
  rphylopic::add_phylopic(amphibians_pic,x = 4, y = 50, ysize = 8, alpha = 1)+
  rphylopic::add_phylopic(fish_pic,      x = 5, y = 50, ysize = 7.5, alpha = 1) +
  theme( legend.position = "bottom",
         axis.text=element_text(size=18),
         axis.title=element_text(size=20),
         legend.title=element_text(size=20),
         legend.text=element_text(size=18))


grob_aves <- grobTree(textGrob(paste0("n = ", sum(data_4_taxa[data_4_taxa$taxa == "aves",]$n)), x=0.1,  y=0.95, hjust=0,
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
fig1 <- fig1 + annotation_custom(grob_aves,xmin = 0.7, xmax = 1.5, ymin = 96, ymax = 103)

fig1 <- fig1 + annotation_custom(grob_reptiles,xmin = 1.7, xmax = 2.5, ymin = 96, ymax = 103)

fig1 <- fig1 + annotation_custom(grob_mammals,xmin = 2.7, xmax = 3.5, ymin = 96, ymax = 103)

fig1 <- fig1 + annotation_custom(grob_amphibians,xmin = 3.7, xmax = 4.5, ymin = 96, ymax = 103)

fig1 <- fig1 + annotation_custom(grob_fishes,xmin = 4.7, xmax = 5.5, ymin = 96, ymax = 103)

ggsave(file = here::here("figures/Figure_1.png"),fig1,width = 12, height = 10, units= "in",dpi= 300)
}
#'---------------------------------------------------------------------@variable_importance

#figure4 <- function(data =  data_noNA, model = test_IUCN[[1]]){ 
#partial_plot <- var_partial(data      =  data,
#                            model = model) 

# Plot importance plot
#importance_plot = var_imp(model)  

#fig <- importance_plot + 
#  (partial_plot[[1]] /partial_plot[[3]])+ 
#  (partial_plot[[2]]/partial_plot[[4]])


#ggsave(file = here::here("figures/Figure_4.png"),fig,width = 12, height = 6, units= "in",dpi= 300)
#}


figure4 <- function(data = output_importance_pd){ 
partial_plot <- var_partial(data) 

# Plot importance plot
importance_plot = var_imp(data)  

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
levels(MPA_FINAL$IUCN) <- c(levels(MPA_FINAL$IUCN), c("NS","NT","TH"))
MPA_FINAL$IUCN[MPA_FINAL$IUCN == 'Threatened'] <- 'TH'
MPA_FINAL$IUCN[MPA_FINAL$IUCN == 'Non Threatened'] <- 'NT'
MPA_FINAL$IUCN[MPA_FINAL$IUCN == 'No Status'] <- 'NS'

MPA_FINAL$IUCN <- factor(MPA_FINAL$IUCN, levels = c("TH","NT","NS"))


#MPA_FINAL$category <- as.factor(paste(MPA_FINAL$IUCN, MPA_FINAL$What, sep="_"))
#MPA_FINAL$category <- factor(MPA_FINAL$category,levels=c("No Status_BEFORE","No Status_AFTER","Non Threatened_BEFORE","Non Threatened_AFTER","Threatened_BEFORE","Threatened_AFTER")) 

MPA_FINAL$category <- as.factor(paste(MPA_FINAL$IUCN, MPA_FINAL$What, sep="_"))
MPA_FINAL$category <- factor(MPA_FINAL$category,levels=c("NS_BEFORE","NS_AFTER","NT_BEFORE","NT_AFTER","TH_BEFORE","TH_AFTER")) 

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
  ylab("Target achievement (%)") + xlab(" ")

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
  ylab("Protection cover (%)") + xlab(" ")

fig <- gridExtra::grid.arrange(b,a,ncol=2)

ggsave(file = here::here("figures/Figure_6.png"),fig,width = 8, height = 4, units= "in",dpi= 300)

}


################### significance tests

BEFORE <- cbind.data.frame(perc_cover=MPA_Protect$perc_cover, Target_achievement_I_IV=MPA_Protect$Target_achievement_I_IV,IUCN=MPA_Protect$IUCN_cat,What="BEFORE")
AFTER <- cbind.data.frame(perc_cover=MPA_Protect$perc_cover, Target_achievement_I_IV=MPA_Protect$Target_achievement_I_IV,IUCN=MPA_Protect$IUCN_final,What="AFTER")
MPA_FINAL <- rbind(BEFORE,AFTER)

MPA_FINAL$What <- factor(MPA_FINAL$What, levels = c("AFTER","BEFORE"))
MPA_FINAL$IUCN <- factor(MPA_FINAL$IUCN, levels = c("Threatened","Non Threatened","No Status"))
MPA_FINAL$category <- as.factor(paste(MPA_FINAL$IUCN, MPA_FINAL$What, sep="_"))
MPA_FINAL$category <- factor(MPA_FINAL$category,levels=c("No Status_BEFORE","No Status_AFTER","Non Threatened_BEFORE","Non Threatened_AFTER","Threatened_BEFORE","Threatened_AFTER")) 



# significance tests: target achievement
options(digits = 3, scipen = 5)

## TARGET ACHIEVEMENT

# Before - after -> paired data
NS <- wilcox.test(MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "No Status_BEFORE"],
                  MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "No Status_AFTER"])

Threat <- wilcox.test(MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "Threatened_BEFORE"],
                      MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "Threatened_AFTER"])

NT <- wilcox.test(MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "Non Threatened_BEFORE"],
                  MPA_FINAL$Target_achievement_I_IV[MPA_FINAL$category == "Non Threatened_AFTER"])

# Intra group:
m_before <- kruskal.test(BEFORE$Target_achievement_I_IV ~ BEFORE$IUCN)
dt_before <- FSA::dunnTest(BEFORE$Target_achievement_I_IV~BEFORE$IUCN)
dt_before$res


m_after <- kruskal.test(AFTER$Target_achievement_I_IV ~ AFTER$IUCN)
dt_after <- FSA::dunnTest(AFTER$Target_achievement_I_IV~AFTER$IUCN)
dt_after$res

## PERCENT COVER

# Before - after -> paired data
NS <- wilcox.test(MPA_FINAL$perc_cover[MPA_FINAL$category == "No Status_BEFORE"],
                  MPA_FINAL$perc_cover[MPA_FINAL$category == "No Status_AFTER"])
Threat <- wilcox.test(MPA_FINAL$perc_cover[MPA_FINAL$category == "Threatened_BEFORE"],
                      MPA_FINAL$perc_cover[MPA_FINAL$category == "Threatened_AFTER"])
NT <- wilcox.test(MPA_FINAL$perc_cover[MPA_FINAL$category == "Non Threatened_BEFORE"],
                  MPA_FINAL$perc_cover[MPA_FINAL$category == "Non Threatened_AFTER"])

# Intra group:
m_before <- kruskal.test(BEFORE$perc_cover ~ BEFORE$IUCN)
dt_before <- FSA::dunnTest(BEFORE$perc_cover~BEFORE$IUCN)
dt_before$res


m_after <- kruskal.test(AFTER$perc_cover ~ AFTER$IUCN)
dt_after <- FSA::dunnTest(AFTER$perc_cover~AFTER$IUCN)
dt_after$res

#All
m_All <- kruskal.test(MPA_FINAL$perc_cover ~ MPA_FINAL$IUCN)
dt_All <- FSA::dunnTest(MPA_FINAL$perc_cover~MPA_FINAL$IUCN)
dt_All$res

# sample sizes
table(MPA_FINAL$category)



