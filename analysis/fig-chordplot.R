#' Circular Chord Diagram

dat_network <- data.frame(data_zonation[ , c("species", "IUCN_cat",
                                             "predict_complementary")])

dat_network <- dat_network[dat_network$"species" %in% MPA_Protect$"species", ]


dat_network <- addLevel(dat_network, "Threatened")
dat_network <- addLevel(dat_network, "Non Threatened")
dat_network <- addLevel(dat_network, "No Status")


for (i in 1:ncol(dat_network)) {
  dat_network[ , i] <- as.factor(as.character(dat_network[ , i]))
}


dat_network <- as.data.frame(sapply(dat_network, mapvalues, 
                                    from = c("Thr"), to = c("Threatened")))

dat_network <- as.data.frame(sapply(dat_network, mapvalues, 
                                    from = c("NThr"), to = c("Non Threatened")))


dat_network$"IUCN_final" <- NA

for (i in 1:nrow(dat_network)) {
  if (is.na(dat_network$"IUCN_cat"[i])) {
    
    dat_network$"IUCN_final"[i] <- dat_network$"predict"[i]
    
  } else {
    
    dat_network$"IUCN_final"[i] <- dat_network$"IUCN_cat"[i]
  }
}


## Replace NA IUCN status ----

dat_network[is.na(dat_network$"IUCN_final"), "IUCN_final"] <- "No Status"
dat_network[is.na(dat_network$"IUCN_cat"), "IUCN_cat"]     <- "No Status"

pos <- which(dat_network$'IUCN_final' == "NaN")
if (length(pos)) dat_network[pos, "IUCN_final"] <- "No Status"

for (i in 1:nrow(dat_network)) {
  if (is.na(dat_network$"predict_complementary"[i]) && 
      dat_network$"IUCN_cat"[i] == "No Status") {
    dat_network$"predict_complementary"[i] <- "No Status"
  }
}

dat_network$"IUCN_cat"   <- factor(dat_network$"IUCN_cat", 
                                 levels = c("Threatened", "No Status", "Non Threatened"))
dat_network$"IUCN_final" <- factor(dat_network$"IUCN_final", 
                                 levels = c("Threatened", "No Status", "Non Threatened"))

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

png(here::here("figures", "figure_2bis.png"), height = 6, width = 6, 
    units = "in", res = 300)


par(new = FALSE, fg = "black", col = "black")
circlize::circos.clear()
circlize::circos.par(start.degree = -90, canvas.xlim = c(-1.2, 1.2), 
                     canvas.ylim = c(-1, 1.25), points.overflow.warning = FALSE)
circlize::chordDiagram(mat, grid.col = grid.col1a, 
                       row.col = c("#FC4E0700", "#E7B800FF", "#00AFBB00"),
                       big.gap = 5,
                       annotationTrack = c("grid"),
                       order = ordre, directional = -1, 
                       annotationTrackHeight = c(0.20))

segments(x0 = 0, y0 = -1, y1 = 1, lty = 2, col = "#00000080")

text(x = -1.15, y = 1.15, labels = "Before ML/DL", pos = 4, cex = 1, font = 2)
text(x = 1.15, y = 1.15, labels = "After ML/DL", pos = 2, cex = 1, font = 2)

par(new = TRUE, fg = "transparent", col = "transparent")

circlize::chordDiagram(mat, grid.col = grid.col1a, 
                       row.col = c("#FC4E0733", "#E7B80000", "#00AFBB33"),
                       big.gap = 5,
                       annotationTrack = c("grid"),
                       order = ordre, directional = -1, 
                       annotationTrackHeight = c(0.20))

for(si in circlize::get.all.sector.index()) {
  
  xlim = circlize::get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  ylim = circlize::get.cell.meta.data("ylim", sector.index = si, track.index = 1)
  
  val <- paste0(perc_values[names(perc_values) == si], "%")
  lab <- gsub("A_|B_", "", si)
  
  circlize::circos.text(mean(xlim), mean(ylim), val, sector.index = si, track.index = 1, 
                        facing = "bending.inside", niceFacing = TRUE, col = "black", 
                        font = 1, adj = c(0.5, 0.5))
  
  circlize::circos.text(mean(xlim), mean(ylim), lab, sector.index = si, track.index = 1, 
                        facing = "bending.inside", niceFacing = TRUE, col = "black", 
                        font = 1, adj = c(0.5, -2))
}

dev.off()
