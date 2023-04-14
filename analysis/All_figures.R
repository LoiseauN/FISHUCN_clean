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
                                     rep("marine fishes", nrow(FB_final))),
                          
                          status = as.factor(c(all_status$rlCodes,
                                               as.character(FB_final$IUCN))),
                          nb_sp = c(all_status$n,rep(1,nrow(FB_final))))


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
  xlab("Taxa")+ylab("Percentage")+
  rphylopic::add_phylopic(birds_pic,     x = 1, y = 50, ysize = 13, alpha = 1)+
  rphylopic::add_phylopic(reptile_pic,     x = 2, y = 50, ysize = 10, alpha = 1)+
  rphylopic::add_phylopic(mammals_pic,   x = 3, y = 50, ysize = 10, alpha = 1)+
  rphylopic::add_phylopic(amphibians_pic,x = 4, y = 50, ysize = 8, alpha = 1)+
  rphylopic::add_phylopic(fish_pic,      x = 5, y = 50, ysize = 7.5, alpha = 1) +
  theme( legend.position = "bottom",
         axis.text=element_text(size=16),
         axis.title=element_text(size=18,face="bold"),
         legend.title=element_text(size=16,face="bold"),
         legend.text=element_text(size=14))

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
fig1

ggsave(file = here::here("figures/Figure1.png"),fig1,width = 12, height = 10, units= "in",dpi= 300)

#'---------------------------------------------------------------------@variable_importance

partial_plot <- var_partial(data =  data_noNA,
                            var = c("DistrArea" , "Max_length","K","Env_2"),
                            names = c("Range size (log)","Max Length (log)", "Growth rate",
                                      "Position in the water column")) 


importance_plot = var_imp(test_IUCN[[1]])      

#importance_plot <- importance_plot + annotation_custom(ggplotGrob(partial_plot[[1]]), xmin = 6, xmax = 11, 
#ymin = 20, ymax = 35.65)

#fig2 <- gridExtra::grid.arrange(partial_plot[[1]],
#                                partial_plot[[2]],
#                                partial_plot[[3]],
#                                partial_plot[[4]])

#fig2 <-  gridExtra::grid.arrange(importance_plot,fig2,ncol=3)
fig2 <- importance_plot + (partial_plot[[1]] /
                   partial_plot[[2]])+ 
                  (partial_plot[[3]] /
                   partial_plot[[4]])
   # importance_plot + annotation_custom(ggplotGrob(partial_plot[[2]]), xmin = 1, xmax = 6, 
# ymin = 20, ymax = 35.65)

ggsave(file = here::here("figures/Figure2.png"),fig2,width = 12, height = 6, units= "in",dpi= 300)



#'---------------------------------------------------------------------@Percentagegainmodel
#'( ( valeur d'arrivée - valeur de départ ) / valeur de départ ) x 100
load(file = here::here("outputs", "dat_network.RData"))

gainNThr <- ((table(dat_network$IUCN_cat)[3]-table(dat_network$IUCN_final)[3])/table(dat_network$IUCN_cat)[3])*100
gainThr <- ((table(dat_network$IUCN_cat)[1]-table(dat_network$IUCN_final)[1])/table(dat_network$IUCN_cat)[1])*100

#'---------------------------------------------------------------------@Checkperfamily
load(file = here::here("outputs", "FB_final.RData"))

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







