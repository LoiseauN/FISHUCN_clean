#' This script draw figure 7
#'
#' @author  Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com},
#'
#' @date 2021/02/17
#' @data = all_geo_res
#' @sup = TRUE or FALSE


figRank <- function(data,sup){ 


  data <- na.omit(data)
  #compute log richness and order
  data$logrichness <- log10(data$richness)  
  data <- data[data$richness>0,]
  data <- data[order(data$richness,decreasing=FALSE),]

  
  if(sup == TRUE){
    data$rankSc2 <- data$DeltaRank_Proba
    data$rankSc1 <- data$IUCN_weigth 
    
  }else{
    data$rankSc2 <- data$Predict_IUCN_same_weigth
    data$rankSc1 <- data$IUCN_weigth 
        }

  data$Delta_rank <- data$rankSc2-data$rankSc1



#sub sample if needed (useful to plot the figure faster but not
# done for the final figure)
#

  #FIGURE A----
  
  ##main figure 
  ##plot the ranks before & after and use the logrichness to color the points 
  my_cols <- colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)
  
  A_main <-   ggplot(data, aes(x=rankSc1, y=rankSc2, colour = logrichness)) +
    geom_point()+theme_bw()+
    scale_colour_gradientn(name  ="log10(richness)",colours = rev(brewer.pal(n = 8, name = "RdBu")))+
    xlab("Rank before")+
    ylab("Rank after") +
    theme(legend.position = c(0.12, 0.88),legend.key.size = unit(0.8, 'cm'),
          legend.background = element_rect(fill='transparent'),
          axis.title=element_text(size=16)) +
    geom_abline(intercept = 0, slope = 1,color="#757575",linetype = "dashed",linewidth = 1)
  
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
  ###use the log of the density to generate a smoother gradient on the figure
  data$Delta_rank_den_log <- log10(data$Delta_rank_den)
  
  A_second <- ggplot(data, aes(logrichness, Delta_rank,color = Delta_rank_den_log)) +
    geom_point(alpha=0.5,size=0.3)+theme_minimal()+
    scale_colour_gradientn(colours = rev(brewer.pal(n = 8, name = "YlGnBu")))+
    geom_quantile(quantiles = c(0.1,0.9),method = "rqss",lambda = 10,linetype="solid",linewidth=1.5,colour="#F78B8B",alpha=0.8)+
    theme(legend.position = "none",panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+
    ylab("Rank after - rank before") +
    xlab("log10(richness)")+
    ylim(-1000000,1700000)+
    geom_hline(yintercept=0, linetype="dashed",color="#4a4949")
  
  ##assemble the figure A and save if needed 
  
  fig_A <- A_main+annotation_custom(ggplotGrob(A_second), xmin = 2150000, xmax = max(data$rankSc1)+150000,ymin = 100, ymax = 1300000)
  #ggsave(here::here("tables_figures","fig_7a.png"), plot = fig_A,
  #       width = 10, height = 10, dpi = 200, units = "in", device='png')
  
  #----
  
  #FIGURE B---- 
  
  #divide the data set into positiv and negative delta ranks 
  data$deltaneg <- NA
  data$deltapos <- NA
  data$deltapos[data$Delta_rank>0]=data$Delta_rank[data$Delta_rank>0]
  data$deltaneg[data$Delta_rank<=0]=data$Delta_rank[data$Delta_rank<=0]
  
  ##Main figure 
  ##plot the delta ranks along the lattitudinal gradient and use the logrichness to color the points 
  
  B_main <- ggplot(data, aes(x=deltaneg, y=lat, colour = logrichness)) +
    geom_point()+theme_bw()+
    scale_colour_gradientn(name  ="log10(richness)",colours = rev(brewer.pal(n = 8, name = "RdBu")))+
    geom_smooth(aes(x = deltaneg),orientation = "y",method="gam",linetype="solid",linewidth=1.7,colour="#5E5E5E",alpha=0.8)+
    geom_smooth(aes(x = deltaneg),orientation = "y",method="gam",se=F,linetype="solid",linewidth=0.4,colour="white",alpha=0.9)+
    theme(legend.position = "none",axis.title=element_text(size=16))+
    ylim(-75,85)+xlim(min(data$Delta_rank)-400000,max(data$Delta_rank))+
    scale_y_continuous(breaks=seq(-80,90,20))+
    geom_point(aes(x=deltapos, y=lat, colour = logrichness))+
    geom_smooth(aes(x = deltapos),orientation = "y",method="gam",linetype="solid",linewidth=1.7,colour="#5E5E5E",alpha=0.8)+
    geom_smooth(aes(x = deltapos),orientation = "y",method="gam",se=F,linetype="solid",linewidth=0.4,colour="white",alpha=0.9)+
    xlab("Rank after - rank before")+ylab("Lattitude")+
    geom_vline(xintercept = 3,color = "#757575", linetype = "dashed",linewidth = 1)+
    geom_hline(yintercept = 0,color = "#757575", linetype = "dashed",linewidth = 1)
  
  ##Secondary figure 
  ##Plot the latitudinal gradient of species richness and use the density of points as color gradient
  
  data$logrichness_den <- get_density(data$logrichness, data$lat, n = 100)
  
  B_second <- ggplot(data, aes(x=logrichness, y=lat, colour = logrichness_den)) +
    geom_point(alpha=0.5,size=0.3)+theme_minimal()+
    scale_colour_gradientn(colours = rev(brewer.pal(n = 8, name = "YlGnBu")))+
    geom_smooth(aes(x = logrichness),orientation = "y",method="gam",linetype="solid",linewidth=1.5,colour="#F78B8B",alpha=0.8)+
    theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ylim(-75,85)+scale_y_continuous(breaks=seq(-80,90,20))+
    xlab("log10(richness)")+ylab("Lattitude")+
    geom_hline(yintercept = 0,color = "#757575", linetype = "dashed",linewidth = 0.5)
  
  ##assemble the figure B and save if needed 
  
  fig_B <-B_main+annotation_custom(ggplotGrob(B_second), xmin = min(data$Delta_rank)-500000, xmax = min(data$Delta_rank)+450000,ymin = 25, ymax = 85)
  #ggsave(here::here("tables_figures","fig_7b.png"), plot = fig_B,
  #       width = 10, height = 10, dpi = 200, units = "in", device='png')
  
  #----
  
  #FIGURE 7ab 
  #Assemble and save the figure 7

fig_7 <- gridExtra::arrangeGrob(fig_A,fig_B,ncol=2)
if(sup == TRUE){ ggsave(here::here("figures","fig_7_supplementary.png"), plot = fig_7,
       width = 20, height = 10, dpi = 300, units = "in", device='png')
  
}else{
    
  ggsave(here::here("figures","fig_7.png"), plot = fig_7,
         width = 20, height = 10, dpi = 300, units = "in", device='png') }

}
