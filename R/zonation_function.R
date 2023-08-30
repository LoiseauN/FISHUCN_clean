# produce Z input files 
remrule1or2 <- function(removalrule){
  if(length(removalrule) != 1){
    print("specify removal rule name, CAZ or ABF?" )
  }
  else if (removalrule == "CAZ"){
    return(1)
  }
  else if (removalrule == "ABF"){
    return(2)
  }
}

write.datfile <- function(filename, usemask = F, maskfile, warp = 1000, removalrule = c("CAZ", "ABF")){
  removalcode <- remrule1or2(removalrule)
  if (!usemask){
    filename <- paste0(removalrule, "_opt.dat")
    settings_file <- file(filename)
    writeLines(c(
      "[Settings]",
      paste0("removal rule = ", removalcode),
      paste0("warp factor = ", warp),
      "edge removal = 1"
    ), settings_file)
    close(settings_file)
  }
  else if (usemask){
    maskname <- gsub(".tif", "", maskfile)
    filename <- paste0(removalrule, "_", maskname, ".dat")
    settings_file <- file(filename)
    writeLines(c(
      "[Settings]",
      paste0("removal rule = ", removalcode),
      paste0("warp factor = ", warp),
      "edge removal = 1",
      "use mask = 1",
      paste0("mask file = ", maskfile)
    ), settings_file)
    close(settings_file)
  }
}



# binary weights: 1 for features to prioritize (focalftrs), 0 for features to keep track of (nonfocal)
write.sppfile.binary <- function(spp.name, focal.files, all.files){
  nonfocal <- setdiff(all.files, focal.files)
  z <- matrix(ncol =6, nrow = length(all.files))
  z[,1] <- c(rep(1.0, length(focal.files)), rep(0, length(nonfocal)))
  z[,c(2:5)] <- 1.0
  z[,6] <- c(as.character(focal.files), as.character(nonfocal))
  write.table(z, file=paste0(spp.name, ".spp"), col.names=FALSE,row.names=FALSE, sep="\t",quote=FALSE)
}


write.sppfile.weights<- function(spp.name, weights.df){
  z <- matrix(ncol =6, nrow = nrow(weights.df))
  z[,1] <- c(weights.df[, 2])
  z[,c(2:5)] <- 1.0
  z[,6] <- c(as.character(weights.df[, 1]))
  write.table(z, file=paste0(spp.name, ".spp"), col.names=FALSE,row.names=FALSE, sep="\t",quote=FALSE)
  
}




dbf2raster <- function(dbfdata, mask.dir){
  ### transform  dataframe (dbfdata) to raster layer according to reference grid (mask) 
  # dbf data = table with GRIDCODE (pixelID),  PERCENTAGE of cell covered
  # cells with more : protected == value of 1
  dbf.file <- list.files(mask.dir, pattern = "dbf")
  dbf.ref <- read.dbf(paste0(mask.dir, "/", dbf.file))
  maskr.file <- list.files(mask.dir, pattern = "img")
  mask <- raster(paste0(mask.dir, "/", maskr.file))
  data2rasterize <- dbfdata[, c("GRIDCODE", "PERCENTAGE")]
  data2rasterize$GRIDCODE <- as.character(data2rasterize$GRIDCODE)
  rownames(data2rasterize) <- data2rasterize$GRIDCODE
  rasterr <- mask # initialiser le raster N2000 à partir du raster de référence
  
  # # reference dbf
  mergedDF <- merge(dbf.ref, data2rasterize, by = "GRIDCODE", all = TRUE)
  mergedDF[is.na(mergedDF)] <- 0
  mergedDF$ID <- NULL
  
  xx <- values(rasterr) # numeros de pixels (= indices) - on  remplace cette valeur par celle des comptages d'esp?ces
  
  xx[!is.na(xx)] <- mergedDF$PERCENTAGE
  
  values(rasterr) <- xx
  return(rasterr)
}




# transform a map of values scaled between 0 and 1 to a map with integer values only = rank of the cell
scaled2rankedr <- function(Zoutputfile){
  outputr <- raster(Zoutputfile)
  xx <- values(outputr)
  ranked.values <- xx
  ranked.values[!is.na(ranked.values)] <- rank(xx[!is.na(xx)])
  hist(ranked.values)
  rankr<- outputr
  values(rankr) <- ranked.values
  return(rankr)
  # same map as Z output ; except rankr = only integers, priority rank value of the cell and outputr (Z output) = priority scaled between 0 and 1. 
  
}

#check you are in the right directory first!
writeBatchFile <- function( datfile, sppfile){
  Zrun <- paste0(gsub(".spp", "", sppfile), "_", gsub(".dat","",datfile))
  bat_file <- file(paste0(Zrun,".bat"))
  writeLines(c(paste0("call zig4.exe -r ", datfile, " ", sppfile, " ZonationOUT/", Zrun, ".txt 0.0 0 1.0 1")), ## distribution smoothing  (second to last digit)
             bat_file)
  close(bat_file)
}

# write SH file with runs for combination of every dat file and every spp file you provide (as vectors of the .spp and .dat files in working dir)
writeSHfile <- function(prefix, dat, spp,workdir,bin){
  sink(paste0(prefix, ".sh"))
  cat("#!/bin/bash\n")
  cat("#OAR -n 1\n")
  cat("#OAR -l /nodes=1\n")
  cat("#OAR --project futureweb\n")
  cat(paste0("#OAR --stdout ", prefix, ".out\n"))
  cat(paste0("#OAR --stderr ", prefix, ".err\n\n"))
  cat("#load modules\n")
  cat("source /applis/site/nix.sh\n\n")
  cat("#directories\n")
  cat(paste0("dir=",workdir,"\n"))
  cat(paste0("bin=",bin,"\n","\n"))
  count <- 0
  if (length(spp) * length(dat) >1){
    for (s in spp){
      for (d in dat){
        if (count < length(spp)*length(dat)-1){
          count <- count+1
          Zrun <- paste0(gsub(".spp", "", s), "_", gsub(".dat","",d))
          cat(c(paste0("$bin -r $dir/", d, " $dir/", s, " $dir/ZonationOUT/", Zrun, " 0.0 0 1.0 1 &\n")))
      }
    }
    }
  
  Zrun <- paste0(gsub(".spp", "", tail(spp, 1)), "_", gsub(".dat","",tail(dat, 1)))
  
  cat(c(paste0("time $bin -r $dir/", tail(dat, 1), " $dir/", tail(spp, 1), " $dir/ZonationOUT/", Zrun, " 0.0 0 1.0 1 &\n\n")))
  }
  else {
    Zrun <- paste0(gsub(".spp", "", spp), "_", gsub(".dat","",dat))
    cat(c(paste0("time $bin -r $dir/",dat, " $dir/",spp, " $dir/ZonationOUT/", Zrun, " 0.0 0 1.0 1 &\n\n")))
  }
  cat("wait")
  
  sink()
}



##### PERFORMANCE CURVE #####
df2perfcurve <- function(df, palette){
  Zrun <- deparse(substitute(df)) # extract name of dataframe
  # name of prioritized feature, determining the order with which landscape is protected
  # weird  ES filename  format so weird conditions
  runinfo <- unlist(strsplit(Zrun, "_"))
  if (runinfo[1] == "ES") { 
    
    if (runinfo[4] == "capacity") { 
      feature_priozd <- runinfo[c(3, 1, 4)]
      feature_priozd <- paste(feature_priozd[1], feature_priozd[2], feature_priozd[3])
    }
    else { 
      feature_priozd <- runinfo[c(3, 1)]
      feature_priozd <- paste(feature_priozd[1], feature_priozd[2])
    }
    
    feature_priozd <- gsub("ES", "ecosystem services", feature_priozd)  
    feature_priozd <- gsub("cul", "cultural", feature_priozd)
    feature_priozd <- gsub("reg", "regulating", feature_priozd)
  }
  else {
    feature_priozd <- runinfo[1]
    feature_priozd <- gsub("ListedTetrapods", "threatened tetrapods", feature_priozd)  
  }
  
  # ABF or CAZ cell removal rule 
  algo <- runinfo[2]
  # whether or not we accounted for N2000 sites as a mask in the Zrun  
  landtenure <- tail(runinfo, n = 1) # always last element in character string of Z output filename 
  landtenure <- gsub("N2000", "Extending N2000 sites", landtenure) 
  landtenure <- gsub("optimal", "Optimal scenario", landtenure)  
  landtenure <- gsub("opt", "Optimal scenario", landtenure)  
  
  # make ready for ggplot: gather the 3 features wcr columns into 2: 1 = type of feature (ES or tetrapods), the other = corresponding wcr value
  df <- gather(df, "Feature", "wcr", -LandProtected)
  df$Feature <- as.factor(df$Feature)
  # df$Feature <- relevel(df$Feature, ref = "ES")
  
  # te <- c("lightskyblue", "lightgreen", "gold") # order: ES, all tetrapods, threatened tetrapods
  # plot
  ggperf <- ggplot(df, aes(x = LandProtected*100, y = wcr))
  if (landtenure == "Extending N2000 sites") { 
    ggperf <- ggperf + 
    geom_vline(xintercept = propN2000*100 +5, colour = "gray20", linetype = "dashed")+
      # geom_vline(xintercept = 5, colour = "gray50")+
      annotate("rect", xmin=0, xmax=propN2000*100, ymin=0 , ymax=1, fill="gray85") 
    # annotate("rect", xmin=0, xmax=5, ymin=0 , ymax=1, alpha=0.1, fill="red") + 
    # annotate("rect", xmin=propN2000*100, xmax=propN2000*100 + 5, ymin=0 , ymax=1, alpha=0.1, fill="red")
  }
  # else {  
  #    ggperf <- ggperf+ geom_vline(xintercept = 10, colour = "gray50")
  #   #   
  # }
  ggperf <- ggperf +geom_line(aes(colour = Feature), size = 1.01)  + 
    scale_colour_manual(values=palette) +
    ggtitle(paste0("Performance curve \nPrioritizing ",  feature_priozd , "\n", landtenure , " (", algo, ")"))+
    theme_classic()+
    xlab("Percentage of landscape protected")+
    ylab("Representation of features") + 
    scale_x_continuous(breaks = seq(0, 100, 20))

  ggperf
}

### Merge priority maps ###
# the order is: 1 = gold, 2 = green, 3 = blue, 4 = orange. 
# min = minimum priority rank to plot 
# max = upper limit of the priority ranks to plot
merge_priority_maps <- function(ranks.tifs, min, max, mN2000, optimal = TRUE){
  val <- 3 # initialize: start at 3 because 1 = EU17, 2 = natura2000
  # merge priority ranks into 1 raster
  for (i in 1:length(ranks.tifs)){
    # load raster files 
    r <- raster(ranks.tifs[i]) 
    r[r < min] <- 0
    r[r > max] <- 0
    r[r != 0] <- val
    val <- val+1
    if (i ==  1){
      all <- r
    }
    else{ all <- all + r} # to sum the rasters together 
  }
  # all is a raster that contains all the priority ranks
  all <- crop(all, mN2000)
  if (!optimal){ # if run includes mask of N2000 areas : plot too:
    # initialize map with Natura 2000 + priorities
    all_N2000 <- mask(mN2000, all, maskvalue = 3, updatevalue = 3) # priority == 3 <=> first file given
    for (i in unique(values(all))){
      if (i != 0 & !is.na(i)){
        all_N2000 <- mask(all_N2000, all, maskvalue = i, updatevalue = i)
      }
    }
    all <- all_N2000
  }
  return(all)
}


# from raster with different types of priorities (with merge after cutoff threshold) 
# to dataframe with value of surface per priority type and per EU country
raster2df_mergedPrioritiesperCountry <- function(Scenario, mergedRaster, lower, upper, sp.countries){
  
  list.EU <- raster::extract(mergedRaster, sp.countries) # this should have the value of every cell for each country polygon 
  datafre <- data.frame( country =as.character(sp.countries$NAME), 
                         ncells_tot = NA, # total land surface km2
                         otherN2000 = NA,
                         biodiversity = NA, 
                         cultural = NA,
                         regulating = NA,
                         biodiv_cultural = NA, 
                         biodiv_regulating = NA,
                         cultural_regulating= NA,
                         fullOverlap = NA)
  
  for(i in 1:length(list.EU)){
    print(i) 
    datafre[i, "ncells_tot"]<- sum(table(list.EU[[i]])) # surface of the country in km2
    datafre[i, "otherN2000"] <- table(list.EU[[i]])["1"] # surface covered by N2000 (in the case of IN scenario, )
    datafre[i, "biodiversity"] <- table(list.EU[[i]])["3"] # surface covered by biodiversity priorities km2
    datafre[i, "cultural"] <- table(list.EU[[i]])["4"] # 
    datafre[i, "regulating"] <- table(list.EU[[i]])["5"] # 
    datafre[i, "biodiv_cultural"] <- table(list.EU[[i]])["7"] # 
    datafre[i, "biodiv_regulating"] <- table(list.EU[[i]])["8"] # 
    datafre[i, "cultural_regulating"] <- table(list.EU[[i]])["9"] # 
    datafre[i, "fullOverlap"] <- table(list.EU[[i]])["12"] # 
    print(as.character(sp.countries$NAME[i])) # country name 
  }
  datafre[is.na(datafre)] <- 0
  datafre$country <- as.character(datafre$country)
  datafre[i+1, "country"] <- "EU"
  datafre[i+1, -1] <- colSums(datafre[, -1], na.rm = T)
  datafre$Scenario <- Scenario
  datafre$topX <- (upper-lower)*100
  
  return(datafre)
}


