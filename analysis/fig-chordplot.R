

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

