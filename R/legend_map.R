
#map <- ggplot(world) +
# geom_sf(data = mask.full.polygon, aes(fill = mask.full, color = mask.full))+ #aes(fill = scale(mask.full), color = scale(mask.full))) +
  
  
#  scale_colour_gradientn(na.value = "#66CDAA66", name  = "Rank after - Rank before",
#  colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)],
#  limits = c(min(c(all_geo_res$DeltaRank_SameWeight,all_geo_res$DeltaRank_Proba),na.rm = T),
#             max(c(all_geo_res$DeltaRank_SameWeight,all_geo_res$DeltaRank_Proba),na.rm = T)),
#  breaks=c(min(c(all_geo_res$DeltaRank_SameWeight,all_geo_res$DeltaRank_Proba),na.rm = T), 0,
#           max(c(all_geo_res$DeltaRank_SameWeight,all_geo_res$DeltaRank_Proba),na.rm = T)),
                                  #    labels=c("- 1 million",0,"+ 2 millions")) +                              
  
                           # scale_fill_gradientn(na.value = "#66CDAA66",name  = "Rank after - Rank before",
                           #   colours = colorRampPalette(rev(brewer.pal(n = 8, name = "RdBu")))(100)[-c(2:15, 85:100)],
                           # limits = c(min(c(all_geo_res$DeltaRank_SameWeight,all_geo_res$DeltaRank_Proba),na.rm = T),
                           #            max(c(all_geo_res$DeltaRank_SameWeight,all_geo_res$DeltaRank_Proba),na.rm = T)),
                           #breaks=c(min(c(all_geo_res$DeltaRank_SameWeight,all_geo_res$DeltaRank_Proba),na.rm = T),0,
                           #         max(c(all_geo_res$DeltaRank_SameWeight,all_geo_res$DeltaRank_Proba),na.rm = T)),
                           #labels=c("- 1 million",0,"+ 2 millions")) +
  
                           #geom_sf(data = world, fill = "white", color = "#bebebe", size = 0.1) +
                           #geom_graticules(mol) +
                         #geom_mapframe(mol, colour = "white", size = 2.0) +
  #geom_mapframe(mol, colour = "black", size = 0.4) +
  
                         #ylab(" ") +
                         # xlab(" ") +
  
                         ##ggthemes::theme_map(base_family = "serif") +
                         #theme_bw()+
                         #theme(legend.position = "bottom",#c(0.85, 0.1),
                         # legend.direction = "horizontal",
                         # legend.title    =  element_text(face = "plain", size = 32,vjust = 1,hjust = 0.5),
                         # legend.text     = element_text(face = "plain", size = 32),
                         #  panel.grid.major = element_blank(),
                         # panel.grid.minor = element_blank(),
                         #panel.border = element_blank(),
        #legend.key.height= unit(1.5, 'cm'),
                         # legend.key.width= unit(4, 'cm'),
                         #axis.text=element_text(size=18),
                         # axis.title=element_text(size=20),
                         # legend.box="horizontal") +
                         #guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
                         #     size = guide_legend(title.position="top", title.hjust = 0.5))

                         #ggsave(file = here::here("figures/Figure6b.png"),map,width = 12, height = 8, units= "in",dpi= 300)
