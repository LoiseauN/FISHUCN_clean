#' Generate partial plot
#'

#' @param data_split 
#' 
#' @return A plot in pdf format with the partial plot 
#'
#' @export
#' 
#' 



#' Generate partial plot
#'

#' @param data_split 
#' 
#' @return A plot in pdf format with the partial plot 
#'
#' @export
#' 
#' 

var_partial = function(data,var,names){
 # data <- data_noNA
    data <- data %>%
    dplyr::select("DistrArea",
                "Max_length",
                  "Env_2",
                  "Climate",
                  "Repro.Mode",
                  "Repro.Fertil",
                  "PriceCateg",
                  "BodyShapeI",
                  "Aquarium",
                  "K",
                  "IUCN")
   
    data <- na.omit(data)
  #Creating the model and predicting to test data
  mod = ranger(IUCN ~ ., data = data , probability = F,
               importance ="permutation",num.trees = 1000,mtry = 3)
  
  #ranger::partial(mod, 
  #       pred.var = c("DistrArea"), 
  #      trim.outliers = TRUE, chull = TRUE, parallel = TRUE,
  #     grid.resolution = 30,  paropts = list(.packages = "ranger"))
  
  
  all_partial <- lapply(1:length(var), function(x){
    pd = edarf::partial_dependence(mod, var[x], 
                                   data = train, interaction =F)
    
    #data <- pd %>% pivot_longer(-var[x])
    #colnames(data)[1] <- "variable"
    
    
    part_plot <- ggplot(pd,aes(x=pd[,var[x]],y=Thr)) + #00AFBB"
      geom_smooth(color="#FC4E07",fill="#FC4E07") +
      ylim(0.1,0.9)+
      theme_bw()+
      ylab("Probability")+
      xlab(names[x])+
      theme(legend.position="none")
    
    return(part_plot)
    
  })
  
  #all_partial <- gridExtra::grid.arrange(all_partial[[1]],
  #                                       all_partial[[2]],
  #                                       ncol=1)
  
}
