#' Panel of Two Scatterplots of PCoA Axis
#'
#' This script produces the Loiseau, Mouquet et al.'s 2022 paper Figure 2, i.e.
#' a  scatterplots #' representing functional spaces based on species features.
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas Loiseau, \email{nicolas.loiseau1@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr}
#'
#' @date 2022/03/16


## Parameters ----

n     <- 1                          # ID of the first sub-plot
plots <- list()                     # Sub-plots storage
All_res <- merge(MPA_Protect,dat_network,by="species")
All_res$IUCN_final <- as.factor(All_res$IUCN_final)

All_res <- merge(All_res,data_noNA[,-c(10,13)],by.x="species",by.y="row.names")
for (i in 1: nrow(All_res)){
  if(All_res$IUCN_cat[i] != "No Status") All_res$predict_complementary[i] <- NA
    
}


# Functional traits

All_res$species


  ## Select var  ----
select <- All_res[,c("species","predict_complementary","DistrArea","Max_length","Env_2","Climate",
                      "Repro.Mode","Repro.Fertil","PriceCateg","BodyShapeI",
                      "Aquarium","K","Genus","Family")]
#select <- select[!is.na(select$predict_complementary),]

rownames(select) <- select$species
features <- select[,-c(1:2,13:14)]

features$Env_2  <- factor(features$Env_2 ,levels = c("bathydemersal","demersal","reef-associated","benthopelagic","pelagic","bathypelagic"))
features$Climate <- factor(features$Climate,levels = c("Boreal","Temperate","Subtropical","Tropical"))          
features$PriceCateg <- factor(features$PriceCateg,levels = c("low","medium","high","very high"))
features$Aquarium <- factor(features$Aquarium,levels = c("never/rarely","potential","public aquariums", "show aquarium","commercial"))
features_cat <- data.frame(names(features), c("Q","Q",rep("O",2),rep("N",2),"O","N","O","Q"))
colnames(features_cat) <- c("trait_name", "trait_type")

# Remove distribArea 
features$Env_2  <- ordered(features$Env_2 )
features$Climate  <- ordered(features$Climate)
features$PriceCateg <-ordered(features$PriceCateg)
  features$Aquarium <- ordered(  features$Aquarium)
  ## Compute distance  ----
 dist <- mFD::funct.dist(
  sp_tr         = features,
  tr_cat        = features_cat,
  metric        = "gower",
  scale_euclid  = "noscale",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)
 

 ## Build PCOA  ----
pco_features<-ape::pcoa(dist)
save(pco_features,file = here::here("outputs/pco_features.RData"))


 coord_features <- pco_features$vectors[,c(1:6)]


 coord_features <- merge(coord_features,All_res,by.x="row.names",by.y="species",all.x=T)
 rownames(coord_features)<-coord_features[,1]
 coord_features<-coord_features[,-1]
 

 
 ## Add Paramaters ----
 coord_features$predict_complementary[is.nan(coord_features$predict_complementary)]<-NA
 
 classes <- unique(coord_features$predict_complementary)[-5]
 color_NoStatus <- "#E7B800"             
 color_NoTHR    <- "#00AFBB"            
 color_THR      <- "#FC4E07"             
 color_Other      <- "grey"  
 
 
 color_classes <- c(color_NoTHR,color_NoStatus,color_Other , color_THR)
 names(color_classes) <- classes
 
  ## Add Scatterplot ----
  
  gplot <- ggplot(coord_features, aes(Axis.1, Axis.2, fill = predict_complementary)) +
    
    geom_point(shape = 21, size = 1, colour = "transparent") +
    
   scale_fill_manual(values=color_classes)
  
  
  ## Get Plot Area Coordinates ----
  
  x_lims <- ggplot_build(gplot)$layout$panel_params[[1]]$x.range
  lag_x  <- diff(x_lims) * 0.04
  y_lims <- ggplot_build(gplot)$layout$panel_params[[1]]$y.range
  lag_y  <- diff(y_lims) * 0.04
  
  
  ## Add Convex Hulls ----

  for (classe in classes) {
    
    coords <- coord_features[coord_features$"predict_complementary" == classe, ]
    findHull <- function(data) data[chull(data$Axis.1, data$Axis.2), ]
    
    hull   <- findHull(coords[complete.cases(coords), ])
    
    gplot <- gplot +
      
      geom_polygon(data = hull, alpha = 0.1, size = 2.0, fill = "white",
                   colour = "white") +
      
      geom_polygon(data = hull, alpha = 0.1, size = 1.0, fill = "transparent",
                   colour = color_classes[classe])
  }
  
  
  ## Change Theme ----
  
  gplot <- gplot +
    
    labs(x = paste("PCoA axis 1"),
         y = paste("PCoA axis 2")) +
    
    theme_bw()
  
  
  ## Add  Legend (right panel) ----
  
    gplot <- gplot +
      
      theme(legend.position = c(0.88, 0.88)) +
      
      labs(fill = "Conservation Status")
 
  
  
ggsave(
  filename  = here::here("figures", "PCOA.png"),
  plot      = gplot,
  width     = 12,
  height    = 12,
  units     = "in",
  dpi       = 300
)


