tree_fish <- fishtree::fishtree_complete_phylogeny(
  mc.cores = 4)
tree <-  tree_fish[[1]]

#tree_test <- fishtree_complete_phylogeny(rank = "Acanthuridae")[[1]]

#H<-phytools::nodeHeights(tree)

set_fish <- ape::drop.tip(tree,tree$tip.label[!is.element(tree$tip.label,as.character(dat_network$species))])

#' ---------------------------------------------------------------------------- @Speciesperstatus


dat_phylo <- data.frame(species = dat_network$species,
                        predict_complementary = as.factor(dat_network$predict_complementary_and_unpredictable),
                        Threatened = rep(NA,nrow(dat_network)),
                        Non_Threatened = rep(NA,nrow(dat_network)),
                        No_Status = rep(NA,nrow(dat_network)),
                        Unpredictable = rep(NA,nrow(dat_network))
)




#CHECK ORDER and same species
dat_phylo <- dat_phylo[order(match(dat_phylo$species, dat_network$species)),]

for(i in 1:nrow(dat_phylo)){
  print(i)
  
  if(is.na(dat_phylo$predict_complementary[i])){
    dat_phylo$Non_Threatened[i] <- 0
    dat_phylo$Threatened[i] <- 0
    dat_phylo$No_Status[i] <- 0
    dat_phylo$Unpredictable[i] <- 0}
  
  
  else if(dat_phylo$predict_complementary[i] == "Non Threatened"){
    dat_phylo$Non_Threatened[i] <- 1
    dat_phylo$Threatened[i] <- 0
    dat_phylo$No_Status[i] <- 0
    dat_phylo$Unpredictable[i] <- 0}
  
  else if(dat_phylo$predict_complementary[i] == "Threatened"){
    dat_phylo$Non_Threatened[i] <- 0
    dat_phylo$Threatened[i] <- 1
    dat_phylo$No_Status[i] <- 0
    dat_phylo$Unpredictable[i] <- 0}
  
  else if(dat_phylo$predict_complementary[i] == "No Status") {
    dat_phylo$Non_Threatened[i] <- 0
    dat_phylo$Threatened[i] <- 0
    dat_phylo$No_Status[i] <- 1
    dat_phylo$Unpredictable[i] <- 0
  }
  
  else if(dat_phylo$predict_complementary[i] == "Unpredictable") {
    dat_phylo$Non_Threatened[i] <- 0
    dat_phylo$Threatened[i] <- 0
    dat_phylo$No_Status[i] <- 0
    dat_phylo$Unpredictable[i] <- 1
  }
  

}



dat_phylo_test_d <- na.omit(dat_phylo)

D.phylogeny <- function(ids,proc,permut,status) {
  #proc <- 2
  #permut <- 10
  #ids <- 1:2 (number of tree)
  #status="Threatened"
  
  
  mclapply(ids,function(id) { 
   
    tree <-  tree_fish[[id]]
    set_tree <- ape::drop.tip(tree,tree$tip.label[!is.element(tree$tip.label,as.character(dat_phylo_test_d$species))])
    set_tree$node.label <- NULL
    
    #collapse or resolve multichotomies in phylogenetic trees TODO check that is mean exactely because need it
    set_tree <- di2multi(set_tree)
    
    #Compute D and statistic
    FR_PhyloD <- caper::comparative.data(set_tree, dat_phylo_test_d,"species",na.omit=FALSE)
    if(status == "Threatened" ) {FR_PhyloD <- caper::phylo.d(FR_PhyloD, binvar=Threatened,permut=permut)}
    if(status == "Non_Threatened" ) {FR_PhyloD <- caper::phylo.d(FR_PhyloD, binvar=Non_Threatened,permut=permut)}
    if(status == "No_Status" ) {FR_PhyloD <- caper::phylo.d(FR_PhyloD, binvar=No_Status,permut=permut)}
    if(status == "Unpredictable" ) {FR_PhyloD <- caper::phylo.d(FR_PhyloD, binvar=Unpredictable,permut=permut)}
    #FR_PhyloD <- sensiPhy::miss.phylo.d(set_tree,dat_phylo,binvar=Threatened)

    #The estimated D value
    estimated_D <- FR_PhyloD$DEstimate
    #A p value,giving the result of testing whether D is significantly different from one
    Pval1 <- FR_PhyloD$Pval1
    #A p value, giving the result of testing whether D is significantly different from zero
    Pval0 <- FR_PhyloD$Pval0
    
    Dstat <- data.frame(estimated_D,Pval1,Pval0)
    
    return(Dstat)
    
  },mc.cores= proc)
  
}

phylo_D_Thr <- D.phylogeny(ids = 1:100,proc=5 ,permut=1000, status = "Threatened")
save(phylo_D_Thr,file=here::here("outputs","phylo_D_Thr.RData"))

phylo_D_NonThr <- D.phylogeny(ids = 1:100,proc=5 ,permut=1000, status = "Non_Threatened")
save(phylo_D_NonThr,file=here::here("outputs","phylo_D_NonThr.RData"))

phylo_D_nostatus <- D.phylogeny(ids = 1:100,proc=5 ,permut=1000, status = "No_Status")
save(phylo_D_nostatus,file=here::here("outputs","phylo_D_nostatus.RData"))

phylo_D_unpredictable <- D.phylogeny(ids = 1:100,proc=5 ,permut=1000, status = "Unpredictable")
save(phylo_D_nostatus,file=here::here("outputs","phylo_D_Unpredictable.RData"))


colnames(dat_phylo)[1:2] <- c("label","group")
#
#for(i in 1:nrow(dat_phylo)){
  
# print(i)
  
# if(is.na(dat_phylo$group[i])){next}  
  
# if(dat_phylo$group[i] == "Non Threatened"){dat_phylo$Non_Threatened[i] <- 1  }
  
# if(dat_phylo$group[i] == "Threatened"){dat_phylo$Threatened[i] <- 1}

# if(dat_phylo$group[i] == "No Status") {dat_phylo$No_Status[i] <- 1}
  
# if(dat_phylo$group[i] == "Unpredictable") {dat_phylo$Unpredictable[i] <- 1 }

 
#}


#' ---------------------------------------------------------------------------- @Parameters

n        <-  1                         # ID of first plot
plots    <- list()                     # Subplots storage


#' ---------------------------------------------------------------------------- @Adddata2PhyloObj
dat_phylo <- dat_phylo[dat_phylo$label %in% set_fish$tip.label,]
dat_phylo <- tidytree::as_tibble(dat_phylo)

new_phylo <- tidytree::as_tibble(set_fish)
new_phylo <- treeio::full_join(new_phylo, dat_phylo, by = "label")
new_phylo <- tidytree::as.treedata(new_phylo)


## Add Phylogenetic Tree ----

tree_plot <- ggtree::ggtree(new_phylo, color = "gray25", layout = "circular",
                            ladderize = FALSE, right = TRUE, size = 0.4) +
  
  theme(plot.margin = unit(rep(-2,  4), "cm")) +
  
  #scale_colour_gradientn(colours = color_meandepth)+
  theme(legend.position="none")



## Add point per Status  ----
#pal <- hp(n = 5, house = "Ravenclaw",direction = -1)
pal <- c("#FC4E07", "#E7B800","#00AFBB","grey")

tree_dt <- as.data.frame(tree_plot$data)
tree_dt <- tree_dt[tree_dt$"isTip" == TRUE, ]
tree_dt <- tree_dt[order(tree_dt$"y"), ]

## Add Threatened Points ----
cols <- ifelse(tree_dt$"Threatened" == 0 , NA, pal[1])

tree_dt$"x" <-  max(tree_dt$"x") + 5

tree_plot <- tree_plot +
  
  geom_point(aes(x = x, y = y, color = Threatened), tree_dt, fill = cols, 
             color = "transparent", shape = 21, size = 1)

## Add Non_Threatened Points ----

cols <- ifelse(tree_dt$"Non_Threatened" == 0, NA, pal[2])

tree_dt$"x" <- max(tree_dt$"x") + 15

tree_plot <- tree_plot +
  
  geom_point(aes(x = x, y = y, color = Non_Threatened), tree_dt, fill = cols, 
             color = "transparent", shape = 21, size = 1)


## Add No_Status Points ----

cols <- ifelse(tree_dt$"No_Status" == 0, NA, pal[3])

tree_dt$"x" <- max(tree_dt$"x") + 18

tree_plot <- tree_plot +
  
  geom_point(aes(x = x, y = y, color = No_Status), tree_dt, fill = cols, 
             color = "transparent", shape = 21, size = 1)

## Add Unpredictible Points ----

cols <- ifelse(tree_dt$"Unpredictable" == 0, NA, pal[4])

tree_dt$"x" <- max(tree_dt$"x") + 21

tree_plot <- tree_plot +
  
  geom_point(aes(x = x, y = y, color = No_Status), tree_dt, fill = cols, 
             color = "transparent", shape = 21, size = 1)



## Add Central Histogram ----
species_d <- data.frame(estimated_d   = c( do.call(rbind,phylo_D_NonThr)$estimated_D,
                                           do.call(rbind,phylo_D_Thr)$estimated_D, do.call(rbind,phylo_D_nostatus)$estimated_D),
                        Status = c(rep("Non Threatened", 100), rep("Threatened", 100),rep("No Status", 100)))



hist_plot <- ggplot(species_d, aes(x = estimated_d, 
                                   fill = Status)) +
  
  geom_density(adjust = 1.5) +
  
  scale_x_continuous(limits = c(0, 1)) +
  
  #scale_color_manual(values = c("#E7B800","#00AFBB", "#FC4E07")) +
  
  scale_fill_manual(values = paste0(c("#E7B800","#00AFBB","#FC4E07") 
  )) +#alpha=0.5
  
  theme_light() +
  
  ylab("Density") +
 
  xlab("D-index") +

  theme(
    #axis.title       = element_blank(),
    axis.text        = element_text(size = 12, colour = "grey50"),
    legend.position  = "None"
  ) 


hist_plot <- ggplotGrob(hist_plot)

#tree_plot <- 

tree_plot <- cowplot::ggdraw(tree_plot)

tree_plot <- tree_plot +
  annotation_custom(
    grob = hist_plot,
    xmin = 0.325,
    xmax = 0.65,
    ymin = 0.35,
    ymax = 0.65
  )
#annotation_custom(
#  grob = hist_plot,
#  xmin = 0.365,
#  xmax = 0.565,
#  ymin = 0.40,
#  ymax = 0.60
#)

#xmin = 0.365,
#xmax = 0.565,
#ymin = 0.40,
#ymax = 0.60

plots[[n]] <- tree_plot

n <- n + 1


## Change Theme ----

tree_legend <- ggplot() +
  
  theme_bw() +
  
  theme(
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    line              = element_blank(),
    text              = element_blank(),
    title             = element_blank(),
    
  )



tree_legend <- tree_legend +
  
  scale_x_continuous(limits = c(0, 10)) +
  
  scale_y_continuous(limits = c(1 - 0.25, 1))




## Add Colors Legend ----

coords <- data.frame(
  x       = seq(1.5, 8, by = 3),
  x_text  = seq(1.5, 8, by = 3)+0.2,
  y       = rep(0.9,3) ,
  text    = c("Threatened", "Non Threatened",
              "No Status")
)

yctr <- 2
xs <- seq(0.9, 4, length.out = 1) #length(color_meandepth) + 1)


tree_legend <- tree_legend +
  
  geom_point(
    data     = coords,
    mapping  = aes(
      x      = x,
      y      = y
    ),
    fill     = c(pal[1], pal[2], pal[3]),
    color    = "transparent",
    shape    = 21,
    size     = 6
  ) +
  
  geom_text(
    data     = coords,
    mapping  = aes(
      x      = x_text,
      y      = y,
      hjust  = -2,
      label  = text
    ),
    hjust    = "left",
    size     = 6,
    color    = "grey50",
    family   = "serif"
  ) #+


plots[[n]] <- tree_legend

#n <- n + 1



## Arrange Sub-plots ----

#mat <- matrix(
#  data   = c(rep(1,30),3,2,2,2,3),#,3,2,2,2,3),
#  ncol   = 5,
#  nrow   = 7,
#  byrow  = TRUE
#)






#grobs <- gridExtra::arrangeGrob(
#  plots[[1]], plots[[2]],
#  layout_matrix = mat
#)

#plot(grobs)
## Export Figure ----

ggsave(
  filename  = here::here("figures","phyl_tree.png"),
  plot      = plots[[1]],#grobs,
  width     = 12,
  height    = 12,
  units     = "in",
  dpi       = 600
)

