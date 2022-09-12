tree_fish <- fishtree::fishtree_complete_phylogeny(
  mc.cores = 4)

#' ---------------------------------------------------------------------------- @Speciesperstatus


dat_phylo <- data.frame(species = dat_network$species,
                        predict_complementary = as.factor(dat_network$predict_complementary),
                        Threatened = rep(NA,nrow(dat_network)),
                        Non_Threatened = rep(NA,nrow(dat_network)),
                        No_Status = rep(NA,nrow(dat_network))
)





#CHECK ORDER and same species
dat_phylo <- dat_phylo[order(match(dat_phylo$species, dat_network$species)),]


for(i in 1:nrow(dat_phylo)){
  print(i)
  
  if(is.na(dat_phylo$predict_complementary[i])){
    dat_phylo$Non_Threatened[i] <- 0
    dat_phylo$Threatened[i] <- 0
    dat_phylo$No_Status[i] <- 0}
  
  
  else if(dat_phylo$predict_complementary[i] == "Non Threatened"){
    dat_phylo$Non_Threatened[i] <- 1
    dat_phylo$Threatened[i] <- 0
    dat_phylo$No_Status[i] <- 0}
  
  else if(dat_phylo$predict_complementary[i] == "Threatened"){
    dat_phylo$Non_Threatened[i] <- 0
    dat_phylo$Threatened[i] <- 1
    dat_phylo$No_Status[i] <- 0}
  
  else if(dat_phylo$predict_complementary[i] == "No Status") {
    dat_phylo$Non_Threatened[i] <- 0
    dat_phylo$Threatened[i] <- 0
    dat_phylo$No_Status[i] <- 1
  }
  

}

#There are 5 NaN to remove
dat_phylo[is.na(dat_phylo$Threatened),]$Threatened <- 0
dat_phylo[is.na(dat_phylo$Non_Threatened),]$Non_Threatened <- 0
dat_phylo[is.na(dat_phylo$No_Status),]$No_Status <- 0


#save(dat_phylo,file="dat_phylo.RData")

dat_phylo_test_d <- na.omit(dat_phylo)

D.phylogeny <- function(ids,proc,permut) {
  #proc <- 2
  #permut <- 10
  #ids <- 1:2
  #status="Threatened"
  
  
  mclapply(ids,function(id) { 
   
    tree <-  tree_fish[[id]]
    set_tree <- ape::drop.tip(tree,tree$tip.label[!is.element(tree$tip.label,as.character(dat_phylo_test_d$species))])
    set_tree$node.label <- NULL
    
    #collapse or resolve multichotomies in phylogenetic trees TODO check that is mean exactely because need it
    set_tree <- di2multi(set_tree)
    
    #Compute D and statistic
    FR_PhyloD <- caper::comparative.data(set_tree, dat_phylo_test_d,"species",na.omit=FALSE)
    FR_PhyloD <- caper::phylo.d(FR_PhyloD, binvar=Threatened,permut=permut)
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

phylo_D_Thr <- D.phylogeny(ids = 1:100,proc=5 ,permut=100) 
