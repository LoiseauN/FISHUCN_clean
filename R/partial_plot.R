#' Generate partial plot
#'

#' @param data_split 
#' 
#' @return A plot in pdf format with the partial plot 
#'
#' @export
#' 
#' 

var_partial = function(data_split,var,names){
  
  names = c("Range size (log)","Max Length (log)", "K")
  data_split <- split
#SHuffling data, then Splitting into training and test data
split <- initial_split(data_split[[i]][sample(nrow(data_split[[i]])),], prop = 0.8)

train <- training(split)
test <- testing(split)

#Creating the model and predicting to test data
mod = ranger(IUCN ~ ., data = train , probability = F,
             importance ="permutation",num.trees = 1000,mtry = 3)

#ranger::partial(mod, 
 #       pred.var = c("DistrArea"), 
  #      trim.outliers = TRUE, chull = TRUE, parallel = TRUE,
   #     grid.resolution = 30,  paropts = list(.packages = "ranger"))


all_partial <- lapply(1:length(var), function(x){
  pd = edarf::partial_dependence(mod, var[x], 
                                 data = train, interaction =F)
  
  data <- pd %>% pivot_longer(-var[x])
  colnames(data)[1] <- "variable"
  part_plot <- ggplot(data,aes(x=variable,y=value,colour=name,fill=name)) + 
    geom_smooth() +
    # change name of legend here 
    scale_fill_manual(name="group",values=c("#00AFBB","#FC4E07"))+
    scale_color_manual(name="group",values=c("#00AFBB","#FC4E07"))+
    ylim(0.1,0.9)+
    theme_bw()+
    ylab("Probability")+
    xlab(names[x])+
    theme(legend.position="none")
  
    return(part_plot)
  
})

all_partial <- gridExtra::grid.arrange(all_partial[[1]],
                                       all_partial[[2]],
                                       all_partial[[3]],ncol=1)

ggsave(file = here::here("figures", "partial_plot.pdf"), all_partial,
       width = 6.5 , height =11.5 )

}

