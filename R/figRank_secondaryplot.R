#' This script draw supp for figure 7
#'
#' @author  Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com},
#'
#' @date 2021/02/17
#' @data = all_geo_res



figRank_SUP_secondary <- function(data){ 
  
  
  data <- na.omit(data)
  #data <- data[sample(c(1:nrow(data)), 10000, replace = TRUE),]
  #compute log richness and order
  data$logrichness <- log10(data$richness)
  data <- data[data$richness>0,]
  data <- data[order(data$richness,decreasing=FALSE),]
  
      data$rankSc2 <- data$Predict_IUCN_same_weigth
    data$rankSc1 <- data$IUCN_weigth 

  
  data$Delta_rank <- data$rankSc2-data$rankSc1
  
  
  
  #sub sample if needed (useful to plot the figure faster but not
  # done for the final figure)
  #
  
  #FIGURE A----
  
  ##main figure 
  ##plot the ranks before & after and use the logrichness to color the points 
  my_cols <- colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)
  

  ##secondary figure 
  ##plot logrichness vs Delta_rank and use the as density function to 
  ##color the points
  ##will also draw the quantile regression (10% and 90%) using the rqss method
  ##https://ggplot2.tidyverse.org/reference/geom_quantile.html
  ##https://search.r-project.org/CRAN/refmans/quantreg/html/rqss.html
  
  ###function getdendity
  ###found here : https://slowkow.com/notes/ggplot2-color-by-density/
  get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }
  
  data$Delta_rank_den <- get_density(data$logrichness, data$Delta_rank, n = 100)
  ##use the log of the density to generate a smoother gradient on the figure
  data$Delta_rank_den_log <- log10(data$Delta_rank_den)
  
  A_second <- ggplot(data, aes(logrichness, Delta_rank,color = Delta_rank_den_log)) +
    geom_point(alpha=0.5,size=0.3)+theme_minimal()+
    scale_colour_gradientn(colours = rev(brewer.pal(n = 8, name = "YlGnBu")))+
    scale_x_log10()+
    geom_quantile(quantiles = c(0.1,0.9),method = "rqss",lambda = 10,linetype="solid",linewidth=1.5,colour="#F78B8B",alpha=0.8)+
    theme(legend.position = "none",panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title=element_text(size=18),
          axis.text = element_text(size = 16))+
    ylab("Rank after - rank before") +
    xlab("Species richness")+
    ylim(-1000000,1700000)+
    geom_hline(yintercept=0, linetype="dashed",color="#4a4949")
  
  ##assemble the figure A and save if needed 
  
  fig_A <- A_second
  
  #----
  
  #FIGURE B---- 
  
  #divide the data set into positiv and negative delta ranks 
  data$deltaneg <- NA
  data$deltapos <- NA
  data$deltapos[data$Delta_rank>0]=data$Delta_rank[data$Delta_rank>0]
  data$deltaneg[data$Delta_rank<=0]=data$Delta_rank[data$Delta_rank<=0]
  
 
  ##Plot the latitudinal gradient of species richness and use the density of points as color gradient
  
  data$logrichness_den <- get_density(data$logrichness, data$lat, n = 100)
  
  B_second <- ggplot(data, aes(x=logrichness, y=lat, colour = logrichness_den)) +
    geom_point(alpha=0.5,size=0.3)+theme_minimal()+
    scale_colour_gradientn(colours = rev(brewer.pal(n = 8, name = "YlGnBu")))+
     scale_x_log10()+
    geom_smooth(aes(x = logrichness),orientation = "y",method="gam",linetype="solid",linewidth=1.5,colour="#F78B8B",alpha=0.8)+
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title=element_text(size=16),
          axis.text = element_text(size = 16))+
    ylim(-75,85)+
     xlab("Species richness")+ylab("Lattitude")+
  geom_hline(yintercept = 0,color = "#757575", linetype = "dashed",linewidth = 0.5)
  
  ##assemble the figure B and save if needed 
  
  fig_B <-B_second
  
  #----
  
  #FIGURE 7ab 
  #Assemble and save the figure 7
  
  fig_7_secondary  <- gridExtra::arrangeGrob(fig_A,fig_B,ncol=2)
ggsave(here::here("figures","fig_7_secondary_supplementary.png"), plot = fig_7_secondary,
                          width = 20, height = 10, dpi = 300, units = "in", device='png')
}
    

