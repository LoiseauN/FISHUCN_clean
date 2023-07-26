#' Generate plot of variable importance
#'
#' Based on the test of your model, this function generates a plot of variable importance based on permutation
#'
#' @param rel_inf Data frame with variable importance 
#' 
#' @return A plot in pdf format with the variable importance plot 
#'
#' @export
#' 
#' 

var_imp = function(rel_inf){
  
  #Plot of variable importance
  tot <- sum(rel_inf[,2])
  
  for (i in 1:nrow(rel_inf)){
    rel_inf[i,2] <- (rel_inf[i,2]/tot)*100
    
  }
  
  
  
  rel_inf <- rel_inf %>% 
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
  
 rel_inf %>%
    arrange(importance.mod.) %>%
    tail(20) %>%
    mutate(rowname=factor(rowname, rowname)) %>%
    ggplot( aes(x=rowname, y=importance.mod.,fill=importance.mod.))+ 
       geom_bar(stat="identity", position="dodge")+ 
   harrypotter::scale_fill_hp(option = "Ravenclaw") + 
   scale_x_discrete() +
   coord_flip()+
      xlab("") +
      ylab("Relative importance (%)") +
    theme_bw() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position="none",
      axis.title=element_text(size=15),
      axis.text=element_text(size=15)
      ) 
    
     
  
 # ggsave(file = here::here("figures", "Figure2.pdf"), 
  #       width = 11.7, height = 8.3)
  
}
