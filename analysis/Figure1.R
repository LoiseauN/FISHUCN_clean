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
  ggplot(rel_inf,aes(importance.mod.,reorder(rowname,importance.mod.),fill=rowname))+
    geom_bar(stat='identity')+
    scale_fill_viridis_d(name="Variable")+
    labs(x="Variable importance by permutation",
         y="")+
    theme_bw()
  
  ggsave(file = here::here("figures", "Figure1.pdf"), 
                 width = 11.7, height = 8.3)
  
}
