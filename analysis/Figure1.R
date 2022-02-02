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
  
  rel_inf %>%
    arrange(importance.mod.) %>%
    tail(20) %>%
    mutate(rowname=factor(rowname, rowname)) %>%
    ggplot( aes(x=rowname, y=importance.mod.) ) +
    geom_segment( aes(x=rowname ,xend=rowname, y=0, yend=importance.mod.), color="grey") +
    geom_point(size=3, color="#69b3a2") +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none"
    ) +
    xlab("Variables") +
    ylab("")

  ggsave(file = here::here("figures", "Figure1.pdf"), 
                 width = 11.7, height = 8.3)
  
}





