


## FUNCTION TO COMPUTE THE CONSERVATION TARGET 
target_func <- function(SR, qt, log=TRUE){
  SR_i <- SR
  qt_i <- qt
  
  if(log) {
    SR <- log(SR)
    qt <- log(qt)
  }	
  dat <- data.frame(matrix(c(100,10,qt), ncol=2, dimnames=list(NULL, c("Target", "SR"))))
  lm_target <- lm(Target~SR, data=dat)
  Tar <- predict(lm_target,newdata=as.data.frame(SR))
  Tar[SR_i<=qt_i[1]] <- 100
  Tar[SR_i>=qt_i[2]] <- 10
  return(Tar)
}

PctMPAI_IV$perc_cover <- as.numeric(as.character(PctMPAI_IV$perc_cover))

# -------------------------------------------------------------------------------------------- Compute target achievement
# Correct target with the range of species following  Jones et al. 2020
#  < 10,000 km2, 100% coverage 
#  > 390,000 km2 the target was reduced to 10% coverage,

load(here::here("data/PctMPAI_IV.RData")) 
load(here::here("data/FishDistribArea_all.RData")) 
load(here::here("outputs/dat_network.RData")) 
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


Cover <- ggstatsplot::ggbetweenstats(
  data = MPA_Protect, #data_protected,
  x = IUCN_final,
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
  x = IUCN_final,
  y = log_Target_achievement_I_IV,
  bf.message = F,

)+
  scale_color_manual(values=c("#FC4E07" , "#E7B800","#00AFBB")) +
  labs(
    x = "IUCN Status",
    y = "Log (Target_achievement (I - IV) + 1)"
  ) 

plot_protection <- grid.arrange(Cover,Target_achievement, ncol = 2)



ggsave(file = here::here("figures/Target_achievement.png"),plot_protection,width = 12, height = 6, units= "in",dpi= 300)







