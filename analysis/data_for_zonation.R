#' Prepare data for zonation
#'
#' Choice of the thresold and merging output from deep and Machine
#' 
#' 
#' @return A list with each downsampled dataframe from your data
#'
#' @export
#' 
#' 
#' 


#Last file to send in zonation
create_data_zonation <- function(data, data_predict){ 
  #data = FB_final
  #data_predict = all_predict

  data_zonation <- data.frame(species = rownames(data),
                            IUCN_cat = data$IUCN) 



# ----------  Change 
data_zonation<- data_zonation %>%
  left_join(data_predict,by="species")
data_zonation$proba_select <- as.numeric(data_zonation$proba_select)/100

rescale_threat <-  subset(data_zonation, data_zonation$predict_complementary =="Thr" & is.na(data_zonation$IUCN_cat))
rescale_non_threat <-  subset(data_zonation, data_zonation$predict_complementary =="NThr" & is.na(data_zonation$IUCN_cat))
rescale_threat$proba_rescale <-  rescalex(a=2,b=5,data=rescale_threat$proba_select)
rescale_non_threat$proba_rescale <-  rescalex(a=1,b=2,data=1-rescale_non_threat$proba_select)
  
  rescale_data <- data.frame(species = c(rescale_threat$species ,
                                         rescale_non_threat$species),
                            proba_rescale = c(rescale_threat$proba_rescale ,
                                              rescale_non_threat$proba_rescale))
  
  data_zonation<- data_zonation %>%
    left_join(rescale_data,by="species")

# SCENARIO 1: 
# 1 for everyone
# SCENARIO 2  :  Weighted in function of the machine learning probability with negative influence for non threaten
# NA  by IUCN and predit = 2 (for SM 2 and 1)
# NonThreatened by IUCN  = 1 
# NonThreatened predit = 2-probability
# Threatened by IUCN = 6
# Threatened predit = 2 + probability (range between 2 and 5)

# SCENARIO 2  :  Weighted in function of the machine learning prediction but same weight than IUCN
data_zonation$weight_zonation_IUCN_and_Predict <- NA
data_zonation$weight_zonation_IUCN_alone <- NA
data_zonation$scenario1_NoWeight <- 1
data_zonation$weight_zonation_Predict_sameWeigthIUCN <- NA

  for(i in 1:nrow(data_zonation)){
   
    print(i)
    
    if(data_zonation$IUCN_cat[i] %in% "Thr"){
      data_zonation$weight_zonation_IUCN_alone[i] <- 6
      data_zonation$weight_zonation_IUCN_and_Predict[i] <-  6  
      data_zonation$weight_zonation_Predict_sameWeigthIUCN[i] <-  6  
      }

     else if (data_zonation$IUCN_cat[i] %in% "NThr"){
       data_zonation$weight_zonation_IUCN_alone[i] <- 1
       data_zonation$weight_zonation_IUCN_and_Predict[i] <- 1
       data_zonation$weight_zonation_Predict_sameWeigthIUCN[i] <-  1 
      }

    else{ 
    #Non information at all
    if(is.na(data_zonation$predict_complementary[i])){
      
      data_zonation$weight_zonation_IUCN_alone[i] <- 2
      data_zonation$weight_zonation_IUCN_and_Predict[i] <- 2 # ou 1.5 
      data_zonation$weight_zonation_Predict_sameWeigthIUCN[i] <-  2
    }
    
      else if(data_zonation$predict_complementary[i] %in% "NThr"){
      
      #Non Threatened predict par model
        data_zonation$weight_zonation_IUCN_alone[i] <- 1
        data_zonation$weight_zonation_IUCN_and_Predict[i] <- data_zonation[data_zonation$species %in% data_zonation$species[i],]$proba_rescale
        data_zonation$weight_zonation_Predict_sameWeigthIUCN[i] <-  1
        }
    
      else if(data_zonation$predict_complementary[i] %in% "Thr"){
      
        data_zonation$weight_zonation_IUCN_alone[i] <- 1
        data_zonation$weight_zonation_IUCN_and_Predict[i] <- data_zonation[data_zonation$species %in% data_zonation$species[i],]$proba_rescale
        data_zonation$weight_zonation_Predict_sameWeigthIUCN[i] <-  6
         }
    
      }
    
    }  
  
  
 save(data_zonation,file= here::here("outputs","data_zonation.RData"))
 

 
 IUCNonly <- data_zonation[,c("species","weight_zonation_IUCN_alone")]
 IUCNandpredict <- data_zonation[,c("species","weight_zonation_IUCN_and_Predict")]
 NoWeight <- data_zonation[,c("species","scenario1_NoWeight")] 
 IUCNandpredict_sameweigth <- data_zonation[,c("species","weight_zonation_Predict_sameWeigthIUCN")] 
 
 data_final_zonation <-list(IUCNonly,IUCNandpredict,NoWeight,IUCNandpredict_sameweigth)
 save(data_final_zonation,file= here::here("outputs","data_final_zonation.RData"))
 
#----------------------------------------------------------------
#FOR THR ONLY
IUCNonly_THR_ONLY <- subset(data_zonation,data_zonation$IUCN_cat=="Thr")
IUCNonly_THR_ONLY <- IUCNonly_THR_ONLY[,c("species","scenario1_NoWeight")]
colnames(IUCNonly_THR_ONLY)  <- "weigth"

IUCNandpredict_THR_ONLY <- subset(data_zonation,data_zonation$predict_complementary=="Thr")
IUCNandpredict_THR_ONLY <- IUCNandpredict_THR_ONLY[,c("species","scenario1_NoWeight")]
colnames(IUCNandpredict_THR_ONLY)  <- "weigth"

data_final_zonation_THR_ONLY <-list(IUCNonly_THR_ONLY,IUCNandpredict_THR_ONLY)
save(data_final_zonation_THR_ONLY,file= here::here("outputs","data_final_zonation_THR_ONLY.RData"))

return(data_zonation)
return(data_final_zonation)
return(data_final_zonation_THR_ONLY)

}













