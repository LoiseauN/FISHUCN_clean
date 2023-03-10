# -------------------------------------------------------------------------------------------- Compute target achievement
# Correct target with the range of species following  Jones et al. 2020
#  < 10,000 km2, 100% coverage 
#  > 390,000 km2 the target was reduced to 10% coverage,
library(scales)
library(ggpubr)

load(here::here("data/PctMPAI_IV.RData")) 
load(here::here("data/FishDistribArea_all.RData")) 
load(here::here("outputs/dat_network.RData")) 

PctMPAI_IV$perc_cover <- as.numeric(as.character(PctMPAI_IV$perc_cover))
MPA_Protect <- FishDistribArea_all[FishDistribArea_all$species %in% dat_network$species,]

MPA_Protect$DistrArea <- MPA_Protect$DistrArea/1e+6
MPA_Protect <- merge(MPA_Protect,PctMPAI_IV,by="species",all.x =T)
MPA_Protect$perc_cover <- as.numeric(as.character(MPA_Protect$perc_cover))
MPA_Protect[is.na(MPA_Protect$perc_cover),]$perc_cover <- 0

MPA_Protect$TargetExp <- NA
for (i in 1 : nrow(MPA_Protect)) {   
  print(i)
  if(is.na(MPA_Protect$DistrArea[i]))  {  MPA_Protect$TargetExp[i]  <- NA
  }  else if(MPA_Protect$DistrArea[i]< 1e+05)  { MPA_Protect$TargetExp[i]  <- 100
  }  else if(MPA_Protect$DistrArea[i]> 39e+05) { MPA_Protect$TargetExp[i]  <- 10  
  }  else {  MPA_Protect$TargetExp[i]  <- NA}
}


qt=quantile(MPA_Protect[is.na(MPA_Protect$TargetExp),]$DistrArea, probs=c(0.1, 0.9),na.rm=T)


for(i in 1:nrow(MPA_Protect)) {
  if(is.na(MPA_Protect$TargetExp[i])) {  MPA_Protect$TargetExp[i] <- round(target_func(MPA_Protect[i,"DistrArea"], qt, log=T),3) }
}

MPA_Protect[,"Target_achievement_I_IV"] <- round(100*(MPA_Protect[,"perc_cover"]/MPA_Protect[,"TargetExp"]),3)

MPA_Protect <- merge(MPA_Protect,dat_network,by="species",all.x= T)


MPA_Protect$log_Target_achievement_I_IV <- log10(MPA_Protect$Target_achievement_I_IV+1)
MPA_Protect$log_cover<- log10(MPA_Protect$perc_cover+1)


MPA_Protect$Target_achievement_I_IV_1 <- (MPA_Protect$Target_achievement_I_IV+1)
MPA_Protect$cover_1<- (MPA_Protect$perc_cover+1)



#MY CHOICE FOR PLOTS 


my_comparisons <- list(
  c("Threatened", "No Status"),
  c("No Status", "Non Threatened"),
  c("Threatened", "Non Threatened")
)

options(scipen=10000)


# NEW PLOT 


#Target_BEFORE ---------------------------------------------------------------------------------
Target_BEFORE <- ggviolin(
  data = MPA_Protect, 
  x = "IUCN_cat",
  y = "Target_achievement_I_IV",
  fill = "IUCN_cat",
  palette = c("#FC4E07" , "#E7B800","#00AFBB"),
  add.params = list(fill = "white"),
  add = "boxplot")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        plot.margin = ggplot2::margin(0, 0, 5.5, 5.5)
  ) +
  ylab("BEFORE")+
  scale_y_continuous(
    trans  = compose_trans("log10","reverse"),
    breaks = c(100, 1, 0)
  ) +
  
  geom_jitter(aes(color =IUCN_cat), 
              shape = 16, 
              position = position_jitter(0.2), 
              size = 0.1) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif",label.y = c(-3.4,-3,-2.6),
                     tip.length = 0
  )


#Target_AFTER ---------------------------------------------------------------------------------
Target_AFTER <- ggviolin(
  data = MPA_Protect, 
  x = "IUCN_final",
  y = "Target_achievement_I_IV",
  fill = "IUCN_final",
  palette = c("#FC4E07" , "#E7B800","#00AFBB"),
  add.params = list(fill = "white"),
  add = "boxplot")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = ggplot2::margin(5.5, 0, 0, 5.5)) +
  scale_y_continuous(
    trans  = compose_trans("log10"),
    breaks = c(100, 1, 0)
  ) +
  ylab("AFTER")+
  scale_x_discrete(labels=c("Threatened" = " ", "No Status" = " ",
                            "Non Threatened" = " ")) + 
  geom_jitter(aes(color =IUCN_final), 
              shape = 16, 
              position = position_jitter(0.2), 
              size = 0.1) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif",label.y = c(3,2.6,2.2),
                     tip.length = 0
  )


#Cover_BEFORE ---------------------------------------------------------------------------------
Cover_BEFORE <- ggviolin(
  data = MPA_Protect, 
  x = "IUCN_cat",
  y = "perc_cover",
  fill = "IUCN_cat",
  palette = c("#FC4E07" , "#E7B800","#00AFBB"),
  add.params = list(fill = "white"),
  add = "boxplot")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x=element_blank()) +
  ylab(" ")+
  scale_y_continuous(
    trans  = compose_trans("log10","reverse"),
    breaks = c(100, 1, 0)
  ) +
  geom_jitter(aes(color =IUCN_cat), 
              shape = 16, 
              position = position_jitter(0.2), 
              size = 0.1) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif",label.y = c(-3.4,-3,-2.6),
                     tip.length = 0
  )


#Cover_AFTER ---------------------------------------------------------------------------------
Cover_AFTER <- ggviolin(
  data = MPA_Protect, 
  x = "IUCN_final",
  y = "perc_cover",
  fill = "IUCN_final",
  palette = c("#FC4E07" , "#E7B800","#00AFBB"),
  add.params = list(fill = "white"),
  add = "boxplot")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = ggplot2::margin(5.5, 5.5, 0, 0)) +
  scale_y_continuous(
    trans  = compose_trans("log10"),
    breaks = c(100, 1, 0)
  ) +
  ylab(" ")+
  scale_x_discrete(labels=c("Threatened" = " ", "No Status" = " ",
                            "Non Threatened" = " ")) + 
  # scale_y_log10(breaks = c(0,1,100),trans= "reverse") +
  geom_jitter(aes(color =IUCN_final), 
              shape = 16, alpha = 0.5,
              position = position_jitter(0.2), 
              size = 0.1) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif",label.y = c(3,2.6,2.2),
                     tip.length = 0
  )


ggarrange(Target_AFTER,Cover_AFTER,Target_BEFORE, Cover_BEFORE,nrow = 2,ncol = 2)

#Target_AFTER 
Target_AFTER plot.margin = ggplot2::margin(5.5, 0, 0, 5.5)

#Cover_AFTER
plot.margin = ggplot2::margin(5.5, 5.5, 0, 0)

#Target_BEFORE
plot.margin = ggplot2::margin(0, 0, 5.5, 5.5)

#Cover_BEFORE
plot.margin = ggplot2::margin(0, 5.5, 5.5, 0)



Target_BEFORE
Target_AFTER

#Target_AFTER ---------------------------------------------------------------------------------
Target_AFTER <- ggviolin(
  data = MPA_Protect, 
  x = "IUCN_final",
  y = "Target_achievement_I_IV",
  fill = "IUCN_final",
  palette = c("#FC4E07" , "#E7B800","#00AFBB"),
  add.params = list(fill = "white"),
  add = "boxplot")+
  theme_bw()+ylim(0,100)+
  theme(legend.position = "none",
        axis.title.x=element_blank())+
  scale_y_continuous(
    trans  = compose_trans("log10"), #,
    breaks = c(100, 50, 25, 0)
    #breaks = rev(c(seq(0,100, by = 10)))
  ) +
  ylab("Target achievement") +
   geom_jitter(aes(color =IUCN_final), 
              shape = 16, 
              position = position_jitter(0.2), 
              size = 0.1) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif",#label.y = c(3,2.6,2.2),
                     tip.length = 0
  )

Target_AFTER

#Cover_AFTER ---------------------------------------------------------------------------------
Cover_AFTER <- ggviolin(
  data = MPA_Protect, 
  x = "IUCN_final",
  y = "cover_1",
  fill = "IUCN_final",
  palette = c("#FC4E07" , "#E7B800","#00AFBB"),
  add.params = list(fill = "white"),
  add = "boxplot")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x=element_blank())+
  scale_y_continuous(
    trans  = compose_trans("log10"),
    #breaks = c(100, 1, 0)
    breaks = rev(c(seq(0,100, by = 10)))
  ) + 
  ylab("MPA Cover") +
  geom_jitter(aes(color =IUCN_final), 
              shape = 16, 
              position = position_jitter(0.2), 
              size = 0.1) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif",#label.y = c(3,2.6,2.2),
                     tip.length = 0
  )

ggarrange(Target_AFTER,Cover_AFTER,ncol = 2)

ggpar(Cover_AFTER, ylim=c(0,100))





# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------






Cover <- ggstatsplot::ggbetweenstats(
  data = MPA_Protect, 
  comparisons = list(c("0", "1")),
  x = IUCN_final,
  y = cover_1,
  bf.message = F,
  results.subtitle = F,
  pairwise.display	 = "all"
)+
  scale_color_manual(values=c("#FC4E07" , "#E7B800","#00AFBB")) +
  labs(
    x = "IUCN Status",
    y = "% Cover"
  ) + 
  #scale_y_reverse()+
  coord_trans(y = "log10") + 
  scale_y_log10(breaks = seq(0,25, by = 5))
  
 


Target_achievement <- ggstatsplot::ggbetweenstats(
  data = MPA_Protect, #data_protected,
  x = IUCN_final,
  y = log_Target_achievement_I_IV,
  bf.message = F,

)+
  scale_color_manual(values=c("#FC4E07" , "#E7B800","#00AFBB")) +
  labs(
    x = "IUCN Status",
    y = "Log (Target_achievement (I - IV) + 1)"
  ) + scale_y_reverse()

plot_protection <- grid.arrange(Cover,Target_achievement, ncol = 2)





ggsave(file = here::here("figures/Target_achievement.before.png"),plot_protection,width = 12, height = 6, units= "in",dpi= 300)








Cover <- ggstatsplot::ggbetweenstats(
  data = MPA_Protect, #data_protected,
  x = IUCN_cat,
  y = log_cover,
  bf.message = F
)+
  scale_color_manual(values=c("#FC4E07" , "#E7B800","#00AFBB")) +
  labs(
    x = "IUCN Status",
    y = "Log (% Cover + 1)"
  ) 

Target_achievement <- ggstatsplot::ggbetweenstats(
  data = MPA_Protect, #data_protected,
  x = IUCN_cat,
  y = log_Target_achievement_I_IV,
  bf.message = F,
  
)+
  scale_color_manual(values=c("#FC4E07" , "#E7B800","#00AFBB")) +
  labs(
    x = "IUCN Status",
    y = "Log (Target_achievement (I - IV) + 1)"
  ) 










