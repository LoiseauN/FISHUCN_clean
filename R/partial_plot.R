#' Generate partial plot
#'

#' @param data_split 
#' 
#' @return A plot in pdf format with the partial plot 
#'
#' @export
#' 
#' 
names = c("Range size","Max Lenght", "K")
var_partial = function(data_split,var,names){
  
#SHuffling data, then Splitting into training and test data
split <- initial_split(data_split[[i]][sample(nrow(data_split[[i]])),], prop = 0.8)

train <- training(split)
test <- testing(split)

#Creating the model and predicting to test data
mod = ranger(IUCN ~ ., data = train , probability = F,
             importance ="permutation",num.trees = 1000,mtry = 3)

ranger::partial(mod, 
        pred.var = c("DistrArea"), 
        trim.outliers = TRUE, chull = TRUE, parallel = TRUE,
        grid.resolution = 30,  paropts = list(.packages = "ranger"))


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
    xlab(names[x])
  
    return(part_plot)
  
}
)

