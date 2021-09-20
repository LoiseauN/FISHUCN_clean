
#Names var
#Looping through all our species and getting traits from Camille function
#Output is a dataframe with all traits from the function

FB_scrap = function(){
  
  nameVar <- colnames(get_fishbase_data("Dermatopsis-multiradiatus"))
  
  species_traits <- pbmclapply(1:nrow(FishDistribArea_all), function (i){  #nrow(FishDistribArea_all)
  
  #Saving actual species into a string
  sp = FishDistribArea_all[i,"species"]
  
  sp = str_replace(sp, "_", "-")
  
  #Saving species traits from function into a dataframe
  temp_traits <- tryCatch(get_fishbase_data(sp),error=function(err){result="NA"})
  
  if((is(temp_traits)[1]=="character")=="TRUE") { 
    res<- matrix(NA,1,33)
    colnames(res) <- nameVar
    return(res)} 
  
  return(temp_traits)
  
},mc.cores=8)

species_traits <- do.call(rbind,species_traits)

}
