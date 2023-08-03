#Function to extract all different value necessary for discussion and results

have_number <- function(data = dat_network,  prediction = all_predict){
  
#Describe original data
start <- data.frame(table(dat_network$IUCN_cat))

#Species removed because too much NA
toomuchNA <- data.frame(table(dat_network$predict_complementary_and_unpredictable))

#Species that have not been predicted by machine and deep
not_pred <- nrow(all_predict)-sum(toomuchNA[c(2,3),2])

#Species predicted only by Machine
predicted_machine <- data.frame(table(all_predict$IUCN_machine))
                                
#Species predicted only by Deep
predicted_machine <- data.frame(table(all_predict$IUCN_deep))
                                
#All species predicted  complementary     
all_pred_comp <- data.frame(table(all_predict$predict_complementary))

#All species predicted  consensus     
all_pred_cons <- data.frame(table(all_predict$predict_consensus))
  
#All IUCN status at the end
all_pred <- data.frame(table(dat_network$IUCN_final))

res <- list(start,toomuchNA,not_pred,predicted_machine,predicted_machine,all_pred_comp,all_pred_cons,all_pred)
names(res) <- c("start","toomuchNA","not_pred",
                "predicted_machine","predicted_machine",
                "all_pred_comp","all_pred_cons","all_pred")
return(res)
}