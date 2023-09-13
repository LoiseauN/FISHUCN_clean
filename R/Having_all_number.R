#Function to extract all different value necessary for discussion and results

have_number <- function(data = dat_network,  prediction = all_predict){
load(here::here("outputs/FB_nonselec.Rdata"))
  
  start <- data.frame(table(dat_network$IUCN_cat))  
  
#Describe original data
start_predict <- data.frame(table(dat_network$IUCN_cat))
#Remove the unpredictable
start_predict[start_predict$Var1 == "No Status",2] <- start_predict[start_predict$Var1 == "No Status",2] -
                                      (nrow(FB_nonselec)-sum(table(FB_nonselec$IUCN)))
start_predict[start_predict$Var1 == "Non Threatened",2] <- start_predict[start_predict$Var1 == "Non Threatened",2] - 
                                            table(FB_nonselec$IUCN)["NThr"]
start_predict[start_predict$Var1 == "Threatened",2] <- start_predict[start_predict$Var1 == "Threatened",2] -
                                        table(FB_nonselec$IUCN)["Thr"]

#Species removed because too much NA
toomuchNA <- data.frame(table(dat_network$predict_complementary_and_unpredictable))

#Desagree
no_agree <- table(all_predict$agree)

#Species that have not been predicted by machine and deep
not_pred <- sum(is.na(all_predict$agree))

#Species predicted only by Machine
predicted_machine <- data.frame(table(all_predict$IUCN_machine))
                                
#Species predicted only by Deep
predicted_deep <- data.frame(table(all_predict$IUCN_deep))
                                
#All species predicted  complementary     
all_pred_comp <- data.frame(table(all_predict$predict_complementary))

#All species predicted  consensus     
all_pred_cons <- data.frame(table(all_predict$predict_consensus))
  
#All IUCN status at the end
all_pred <- data.frame(table(dat_network$IUCN_final))


#phylogeny
load(file=here::here("outputs","phylo_D_Thr.RData"))
load(file=here::here("outputs","phylo_D_NonThr.RData"))
load(file=here::here("outputs","phylo_D_nostatus.RData"))
load(file=here::here("outputs","phylo_D_unpredictable.RData"))

phylo <- data.frame(phylot_TH_mean    = mean(do.call(rbind,phylo_D_Thr)[,1]),
                    phylot_TH_sd      = sd(do.call(rbind,phylo_D_Thr)[,1]),
                    phylot_NonTH_mean = mean(do.call(rbind,phylo_D_NonThr)[,1]),
                    phylot_NonTH_sd   = sd(do.call(rbind,phylo_D_NonThr)[,1]),
                    phylot_NS_mean    = mean(do.call(rbind,phylo_D_nostatus)[,1]),
                    phylot_NS_sd      = sd(do.call(rbind,phylo_D_nostatus)[,1]),
                    phylot_Unpredic_mean = mean(do.call(rbind,phylo_D_unpredictable)[,1]),
                    phylot_Unpredic_sd = sd(do.call(rbind,phylo_D_unpredictable)[,1])
                    )


#protection
protection <- data.frame(Target_TH_before =  mean(MPA_Protect[MPA_Protect$IUCN_cat %in% "Threatened",]$Target_achievement_I_IV), 
                         Target_TH_after  =  mean(MPA_Protect[MPA_Protect$IUCN_final %in% "Threatened",]$Target_achievement_I_IV),
                         
                         Target_NonTH_before =  mean(MPA_Protect[MPA_Protect$IUCN_cat %in% "Non Threatened",]$Target_achievement_I_IV),
                         Target_NonTH_after  =  mean(MPA_Protect[MPA_Protect$IUCN_final %in% "Non Threatened",]$Target_achievement_I_IV),
                         
                         Target_NS_before =  mean(MPA_Protect[MPA_Protect$IUCN_cat %in% "No Status",]$Target_achievement_I_IV),
                         Target_NS_after  =  mean(MPA_Protect[MPA_Protect$IUCN_final %in% "No Status",]$Target_achievement_I_IV),
                         
                         Cover_TH_before =  mean(MPA_Protect[MPA_Protect$IUCN_cat %in% "Threatened",]$perc_cover),
                         Cover_TH_after  =  mean(MPA_Protect[MPA_Protect$IUCN_final %in% "Threatened",]$perc_cover),
                         
                         Cover_NonTH_before =  mean(MPA_Protect[MPA_Protect$IUCN_cat %in% "Non Threatened",]$perc_cover),
                         Cover_NonTH_after  =  mean(MPA_Protect[MPA_Protect$IUCN_final %in% "Non Threatened",]$perc_cover),
                         
                         Cover_NS_before =  mean(MPA_Protect[MPA_Protect$IUCN_cat %in% "No Status",]$perc_cover),
                         Cover_NS_after  =  mean(MPA_Protect[MPA_Protect$IUCN_final %in% "No Status",]$perc_cover)
                         )




#'---------------------------------------------------------------------@Percentagegainmodel
#'( ( valeur d'arrivée - valeur de départ ) / valeur de départ ) x 100


gainNThr <- ((table(dat_network$IUCN_cat)["Non Threatened"]-table(dat_network$IUCN_final)["Non Threatened"])/table(dat_network$IUCN_cat)["Non Threatened"])*100
gainThr <- ((table(dat_network$IUCN_cat)["Threatened"]-table(dat_network$IUCN_final)["Threatened"])/table(dat_network$IUCN_cat)["Threatened"])*100
lossNS <- ((table(dat_network$IUCN_cat)["No Status"]-table(dat_network$IUCN_final)["No Status"])/table(dat_network$IUCN_cat)["No Status"])*100

gain_loss <- c(gainNThr,gainThr,lossNS)

res <- list(start,start_predict,toomuchNA,no_agree,not_pred,predicted_machine,
            predicted_deep,all_pred_comp,all_pred_cons,all_pred,
            gain_loss,phylo,protection)
names(res) <- c("start","start_predict","toomuchNA","no_agree","not_pred",
                "predicted_machine","predicted_deep",
                "all_pred_comp","all_pred_cons","all_pred",
                "gain_loss","phylo","protection")
return(res)

}
