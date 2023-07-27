

#' Generate partial plot
#'

#' @param data_split 
#' 
#' @return A plot in pdf format with the partial plot 
#'
#' @export
#' 
#' 
var_partial = function(data,model){
  # data =  data_noNA
  # model = test_IUCN[[1]]
  
  
  pal <- c("#B35800","#A8BCC1","#42A5D9","#3573A4")
  
  data = na.omit(data)
  #Creating the model and predicting to new data
  mod = ranger::ranger(IUCN ~ ., data = data, probability = F,
                       importance ="permutation",num.trees = 1000,mtry = 3)
  
  
  #ranger::partial(mod, 
  #       pred.var = c("DistrArea"), 
  #      trim.outliers = TRUE, chull = TRUE, parallel = TRUE,
  #     grid.resolution = 30,  paropts = list(.packages = "ranger"))
  
 var <- data.frame(mod$variable.importance)
 var <- model[order(model$importance.mod.,decreasing = TRUE),]
  
 name_var <- var %>% 
    mutate(rowname = recode_factor(rowname, "Troph" = "Trophic",
                                   "BodyShapeI" = 'Body Shape' ,
                                   "Aquarium" = "Interest for aquarium",
                                   "Depth_max" = "Depth max",
                                   "Depth_min" = "Depth min",
                                   "DistrArea" = "Range size",
                                   "K" = "Growth rate",
                                   "Habitat" = "Position in water column",
                                   "PriceCateg" = "Price Category",
                                   "ReproMode" = "Reproduction mode",
                                   "RepGuild1" = "Reproductive guild"))
 
 name_var <- c(name_var$rowname[c(1:4)])
  
 var <- data.frame(var[c(1:4),1])
  
 all_partial <- lapply(1:nrow(var), function(x){
    pd = edarf::partial_dependence(mod, var[x,1], 
                                   data = data, interaction =F)
    
      part_plot <- ggplot(pd,aes(x=pd[,var[x,]],y=Thr)) + #00AFBB"
        geom_smooth(color=pal[x],fill=pal[x]) +
        ylim(0,0.5)+
        theme_bw()+
        ylab("Probability")+
        xlab(name_var[x])+
        theme(legend.position="none",
              panel.grid.minor = element_blank(),
              axis.title=element_text(size=15),
              axis.text = element_text(size = 15))
      # }
    return(part_plot)
    
  })
  
  #all_partial <- gridExtra::grid.arrange(all_partial[[1]],
  #                                       all_partial[[2]],
  #                                       ncol=1)
  
}



#data <- pd %>% pivot_longer(-var[x])
#colnames(data)[1] <- "variable"


#if (var[x] == "Env_2"){
#  dftext <- data.frame(pos = 1.2:6.2, var = c("bathy-demersal",
#                                          "bathy-pelagic",
#                                             "bentho-pelagic",
#                                              "demersal",
#                                             "pelagic",
#                                             "reef-associated"))

#   part_plot <- ggplot(pd,aes(x=pd[,var[x]],y=Thr)) + #00AFBB"
#   geom_bar(stat="identity", position="dodge",color=pal[x],fill=pal[x])+ 
#   ylim(0,0.1)+
#   theme_bw()+
#   ylab("Probability")+
#   xlab(names[x])+
#   theme(legend.position ="none",
#         axis.text.x = element_blank(), 
#         axis.ticks.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title=element_text(size=15),
#         axis.text = element_text(size = 15)) +
#   geom_text(data= dftext,
#             mapping= aes(x=pos, y=0.05, label = var),
#             size=3, 
#             angle=90,
#             vjust=-0.4, 
#             hjust=0) 

#} else {

