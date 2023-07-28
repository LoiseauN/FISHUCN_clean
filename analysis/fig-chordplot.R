#' Circular Chord Diagram


chid_chord <- function(sup){ 
  
  load(here::here("outputs", "data_zonation.RData"))
  
  # for consensensus 
    if(sup == TRUE){dat_network <- data.frame(data_zonation[ , c("species", "IUCN_cat",
                                                              "predict_consensus")])  }
  # for complementarity 
  else{dat_network <- data.frame(data_zonation[ , c("species", "IUCN_cat",
                                                    "predict_complementary")]) }

  colnames(dat_network)[3] <- "predict"

## Rename levels ----

for (i in 2:ncol(dat_network)) {
  
  dat_network[ , i] <- as.character(dat_network[ , i])
  
  dat_network[ , i] <- plyr::mapvalues(dat_network[ , i], 
                                       from = "NThr", 
                                       to   = "Non Threatened")
  dat_network[ , i] <- plyr::mapvalues(dat_network[ , i], 
                                       from = "Thr", 
                                       to   = "Threatened")
}


## Create IUCN final column ----

dat_network$"IUCN_final" <- NA

for (i in 1:nrow(dat_network)) {
  
  if (is.na(dat_network$"IUCN_cat"[i])) {
    
    dat_network$"IUCN_final"[i] <- dat_network$"predict"[i]
    
  } else {
    
    dat_network$"IUCN_final"[i] <- dat_network$"IUCN_cat"[i]
  }
}


## Replace missing IUCN status ----

dat_network[is.na(dat_network$"IUCN_final"), "IUCN_final"] <- "No Status"
dat_network[is.na(dat_network$"IUCN_cat"), "IUCN_cat"]     <- "No Status"


## Create levels ----

level_s <- c("Threatened", "No Status", "Non Threatened")

dat_network$"IUCN_cat"   <- factor(dat_network$"IUCN_cat", 
                                 levels = level_s)

dat_network$"IUCN_final" <- factor(dat_network$"IUCN_final", 
                                 levels = level_s)

#save(dat_network, file = here::here("outputs", "dat_network.RData"))

## Confusion matrix ----
mat <- table(dat_network$"IUCN_cat", dat_network$"IUCN_final")

rownames(mat) <- paste0("A_", rownames(mat))
colnames(mat) <- paste0("B_", colnames(mat))

sum_before <- round(100 * apply(mat, 1, sum) / sum(apply(mat, 1, sum)), 1)
sum_after  <- round(100 * apply(mat, 2, sum) / sum(apply(mat, 2, sum)), 1)

perc_values <- c(sum_before, sum_after)


ordre <- c("A_Threatened", "A_No Status", "A_Non Threatened", 
           "B_Threatened", "B_No Status", "B_Non Threatened")

grid.col1a = c(A_Threatened = "#FC4E07", `A_Non Threatened` = "#00AFBB", `A_No Status` = "#E7B800",
               B_Threatened = "#FC4E07", `B_Non Threatened` = "#00AFBB", `B_No Status` = "#E7B800")



if(sup == TRUE) {png(here::here("figures", "figure_2_supp.png"), height = 6, width = 6, 
                    units = "in", res = 300) }

else {png(here::here("figures", "figure_2bis.png"), height = 6, width = 6, 
                    units = "in", res = 300) }

par(new = FALSE, fg = "black", col = "black")
circlize::circos.clear()
circlize::circos.par(start.degree = -90, canvas.xlim = c(-1.2, 1.2), 
                     canvas.ylim = c(-1, 1.25), points.overflow.warning = FALSE)
circlize::chordDiagram(mat, grid.col = grid.col1a, 
                       row.col = c("#FC4E0700", "#E7B80000", "#00AFBB00"),
                       big.gap = 20,
                       annotationTrack = c("grid"),
                       order = ordre, directional = -1, 
                       annotationTrackHeight = c(0.20))

segments(x0 = 0, y0 = -1, y1 = 1, lty = 2, col = "#00000080")

text(x = -0.25, y = 1.15, labels = "BEFORE", pos = 2, cex = 1, font = 1)
text(x =  0.25, y = 1.15, labels = "AFTER", pos = 4, cex = 1, font = 1)

par(new = TRUE, fg = "transparent", col = "transparent")

circlize::chordDiagram(mat, grid.col = grid.col1a, 
                       row.col = c("#E7B80080", "#00AFBB20", "#FC4E0780"),
                       big.gap = 20,
                       annotationTrack = c("grid"), 
                       order = ordre, directional = -1,
                       annotationTrackHeight = c(0.20))

for (si in circlize::get.all.sector.index()) {
  
  xlim = circlize::get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  ylim = circlize::get.cell.meta.data("ylim", sector.index = si, track.index = 1)
  
  val <- paste0(perc_values[names(perc_values) == si], "%")
  lab <- gsub("A_|B_", "", si)
  
  # circlize::circos.text(mean(xlim), mean(ylim), val, sector.index = si, track.index = 1, 
  #                       facing = "bending.inside", niceFacing = TRUE, col = "black", 
  #                       font = 1, adj = c(0.5, 0.5))
  
  circlize::circos.text(mean(xlim), mean(ylim), val, sector.index = si, track.index = 1, 
                        facing = "bending.inside", niceFacing = TRUE, col = "black", 
                        font = 1, adj = c(0.5, -3), cex = 0.75)
}

dev.off()
}
