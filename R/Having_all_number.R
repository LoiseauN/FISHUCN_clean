#Function to extract all different value necessary for discussion and results

have_number <- function(data = dat_network,  prediction = all_predict){
  
  all_predict
#Describe original data
start <- data.frame(table(dat_network$IUCN_cat))

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

#'---------------------------------------------------------------------@Percentagegainmodel
#'( ( valeur d'arrivée - valeur de départ ) / valeur de départ ) x 100


gainNThr <- ((table(dat_network$IUCN_cat)["Non Threatened"]-table(dat_network$IUCN_final)["Non Threatened"])/table(dat_network$IUCN_cat)["Non Threatened"])*100
gainThr <- ((table(dat_network$IUCN_cat)["Threatened"]-table(dat_network$IUCN_final)["Threatened"])/table(dat_network$IUCN_cat)["Threatened"])*100
lossNS <- ((table(dat_network$IUCN_cat)["No Status"]-table(dat_network$IUCN_final)["No Status"])/table(dat_network$IUCN_cat)["No Status"])*100

gain_loss <- c(gainNThr,gainThr,lossNS)

res <- list(start,toomuchNA,no_agree,not_pred,predicted_machine,
            predicted_deep,all_pred_comp,all_pred_cons,all_pred,
            gain_loss)
names(res) <- c("start","toomuchNA","no_agree","not_pred",
                "predicted_machine","predicted_deep",
                "all_pred_comp","all_pred_cons","all_pred",
                "gain_loss")
return(res)

}
