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
  
 # names = c("Range size (log)","Max Length (log)")
  data_split <- split
#SHuffling data, then Splitting into training and test data
split <- initial_split(data_split[[i]][sample(nrow(data_split[[i]])),], prop = 0.8)

train <- training(split)
test <- testing(split)

train <- train %>%
  rename(Survival       = DistrArea,
         Max Length     = Max_length,
         Water Column   = Env_2,
         Climate        = Climate,
         Reproduction   = Repro.Mode,
         Fertility      = Repro.Fertil,
         Price Category = PriceCateg,
         Body Shape     = BodyShapeI,
         Aquarium       = Aquarium,
         Growth rate    = K)

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

