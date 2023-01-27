
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
 # data =  data_noNA
 # var = c("DistrArea" , "Max_length","K") 
 # names = c("Range size (log)","Max Length (log)","Growth rate")

  pal <- hp(n = length(var), house = "Ravenclaw")

  data = na.omit(data)
    #Creating the model and predicting to new data
    mod = ranger::ranger(IUCN ~ ., data = data, probability = F,
                         importance ="permutation",num.trees = 1000,mtry = 3)

  #ranger::partial(mod, 
  #       pred.var = c("DistrArea"), 
  #      trim.outliers = TRUE, chull = TRUE, parallel = TRUE,
  #     grid.resolution = 30,  paropts = list(.packages = "ranger"))
  
  
  all_partial <- lapply(1:length(var), function(x){
    pd = edarf::partial_dependence(mod, var[x], 
                                   data = data, interaction =F)
    
    #data <- pd %>% pivot_longer(-var[x])
    #colnames(data)[1] <- "variable"
    
    
    part_plot <- ggplot(pd,aes(x=pd[,var[x]],y=Thr)) + #00AFBB"
      geom_smooth(color=pal[x],fill=pal[x]) +
      ylim(0,0.5)+
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

