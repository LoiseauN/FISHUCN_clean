

#' Generate partial plot
#'

#' @param data_split 
#' 
#' @return A plot in pdf format with the partial plot 
#'
#' @export
#' 
#' 
var_partial = function(data,var,names){
  #data =  data_noNA
  #var = c("DistrArea" , "Max_length","K","Env_2") 
  #names = c("Range size (log)","Max Length (log)","Growth rate", "Position in the water column")
  
  pal <- c("#B35800","#A8BCC1","#42A5D9","#3573A4")
  
  data = na.omit(data)
  #Creating the model and predicting to new data
  mod = ranger::ranger(IUCN ~ ., data = data, probability = F,
                       importance ="permutation",num.trees = 1000,mtry = 3)
  
  #ranger::partial(mod, 
  #       pred.var = c("DistrArea"), 
  #      trim.outliers = TRUE, chull = TRUE, parallel = TRUE,
  #     grid.resolution = 30,  paropts = list(.packages = "ranger"))
  
  
  all_partial <- lapply(1:length(var), function(x){
    pd = edarf::partial_dependence(mod, var[x], 
                                   data = data, interaction =F)
    
    #data <- pd %>% pivot_longer(-var[x])
    #colnames(data)[1] <- "variable"
    
    
    if (var[x] == "Env_2"){
      dftext <- data.frame(pos = 1.2:6.2, var = c("bathy-demersal",
                                                  "bathy-pelagic",
                                                  "bentho-pelagic",
                                                  "demersal",
                                                  "pelagic",
                                                  "reef-associated"))

        part_plot <- ggplot(pd,aes(x=pd[,var[x]],y=Thr)) + #00AFBB"
        geom_bar(stat="identity", position="dodge",color=pal[x],fill=pal[x])+ 
        ylim(0,0.1)+
        theme_bw()+
        ylab("Probability")+
        xlab(names[x])+
        theme(legend.position ="none",
              axis.text.x = element_blank(), 
              axis.ticks.x = element_blank(),
              panel.grid.minor = element_blank()) +
        geom_text(data= dftext,
                  mapping= aes(x=pos, y=0.05, label = var),
                  size=3, 
                  angle=90,
                  vjust=-0.4, 
                  hjust=0) 
      
    } else {
      
      
      part_plot <- ggplot(pd,aes(x=pd[,var[x]],y=Thr)) + #00AFBB"
        geom_smooth(color=pal[x],fill=pal[x]) +
        ylim(0,0.5)+
        theme_bw()+
        ylab("Probability")+
        xlab(names[x])+
        theme(legend.position="none",
              panel.grid.minor = element_blank())
    }
    return(part_plot)
    
  })
  
  #all_partial <- gridExtra::grid.arrange(all_partial[[1]],
  #                                       all_partial[[2]],
  #                                       ncol=1)
  
}



#Zrank_main$diff <- Zrank_main$rankSc2-Zrank_main$rankSc1

#fig_rank_hex <- ggplot(Zrank_main, aes(x=rankSc1, y=rankSc2) ) +
#  geom_hex(bins = 100) +
#  scale_fill_continuous(type = "viridis") +
#  theme_bw() + xlab("IUCN only")+ ylab("IUCN + Predicted")
#ggsave(file = here::here("figures/fig_rank_hex.png"),width = 12, height = 12, units= "in",dpi= 300)
#all_geo_res <- all_geo_res[,-c(10,11)]
#all_geo_res <- merge(all_geo_res,Zrank_main,by = "ID",all.x=T)
#all_geo_res$DeltaRank <- all_geo_res$rankSc2-all_geo_res$rankSc1

