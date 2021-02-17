who.remote <- function(who){
#VF = Valentine 
#RS = Raphael
#NL = Nicolas
#NL_Home = Nicolas  
   #if (who %in% "LV") {
  #  data_dir <-
   # results_dir <-
    #script_dir <-
  #} 
  
   if (who %in% "RS") {
      
     data_dir <<- file.path("C:","Users","Admin","Documents","Stage","FISHUCN_clean","data")
     
     results_dir <<- file.path("C:","Users","Admin","Documents","Stage","FISHUCN_clean","outputs")
     
     script_dir <<- file.path("C:","Users","Admin","Documents","Stage","FISHUCN_clean","R")
     
     fig_dir <<- file.path("C:","Users","Admin","Documents","Stage","FISHUCN_clean","Figures")
   } 
  
  if (who %in% "NL_Home") {
     
     data_dir <<- file.path("~/Documents/FISHUCN/FISHUCN_clean/data")
     
     results_dir <<- file.path("~/Documents/FISHUCN/FISHUCN_clean/Output")
     
     script_dir <<- file.path("~/Documents/FISHUCN/FISHUCN_clean/R")
     
     fig_dir <<- file.path("~/Documents/FISHUCN/FISHUCN_clean/Figures")
  }
  
  if (who %in% "NL") {
    
    data_dir <<- file.path("~/Documents/Postdoc MARBEC/FISHUCN/FISHUCN/data")
    
    results_dir <<- file.path("~/Documents/Postdoc MARBEC/FISHUCN/FISHUCN/Output")
    
    script_dir <<- file.path("~/Documents/Postdoc MARBEC/FISHUCN/FISHUCN/R")
    
    fig_dir <<- file.path("~/Documents/Postdoc MARBEC/FISHUCN/FISHUCN/Figures")
  }
  
  if (who %in% "VF") {
    
    data_dir <<- file.path()
    
    results_dir <<- file.path()
    
    script_dir <<- file.path()
    
    fig_dir <<- file.path()
  }
  
}

who.remote("RS")
who.remote("NL_Home")
who.remote("NL")

#Fonction to add new level in factor variable
addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}

