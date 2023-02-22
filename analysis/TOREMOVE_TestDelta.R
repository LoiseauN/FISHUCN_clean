CellsIn = exactextractr::exact_extract(mask,mollBorder,include_xy=TRUE, include_cell = TRUE)

dim(CellsIn[[1]])

all_geo_res <- all_geo_res2

all_geo_res <- merge(all_geo_res,CellsIn, by.x = "ID",by.y = "cell" )


all_geo_res  <- all_geo_res[sample(nrow(all_geo_res), 1000), ]
all_geo_res$richness <- all_geo_res$Rthr + all_geo_res$Rnothr + all_geo_res$Rnostatus
all_geo_res$DeltaRank <- all_geo_res$DeltaRank/max(abs(all_geo_res$DeltaRank))

pdf(file= here::here("figures","edgeplot.pdf"),height = 7.2, width = 11 ) 

layout(matrix(c(1,1,1,1,1,2),nrow=6,byrow=TRUE))
# the following line shows what this 2-panel layout looks like: 
# layout.show(2) 

par(mar=c(0,6,2,2)) 
plot(all_geo_res$richness, # x-axis
     all_geo_res$DeltaRank, # y axis
     type="n",yaxt="n",ylim=c(-1,1),
     xlim=c(min(all_geo_res$richness),max(all_geo_res$richness)), # empty plot, sets axis limits.
     xaxt="n",ylab="",lty=1,
     main="Change in prioritization",
     pch=16,cex.main=1.4)

abline(h=0,col="darkgray")

text(x=3000,y=0.05,"positive",col="darkgray",cex=1.3) 
text(x=3000,y=-.05,"negative",col="darkgray",cex=1.3)

abline(v=0, col="lightgray") # gridlines
abline(v=500, col="lightgray") # gridlines
abline(v=1000, col="lightgray")
abline(v=1500, col="lightgray")
abline(v=2000, col="lightgray")
abline(v=2500, col="lightgray")
abline(v=3000, col="lightgray")

coloo <- NA
for (i in 1:nrow(all_geo_res)){
  
  if(all_geo_res$DeltaRank[i]< -0.5) {coloo=wes_palette(name="Zissou1")[1]}
  if(all_geo_res$DeltaRank[i]< 0 & all_geo_res$DeltaRank[i]> -0.5) {coloo=wes_palette(name="Zissou1")[2]}
  if(all_geo_res$DeltaRank[i]< 0.5 & all_geo_res$DeltaRank[i]> 0) {coloo=wes_palette(name="Zissou1")[3]}
  if(all_geo_res$DeltaRank[i] > 0.5) {coloo = wes_palette(name="Zissou1")[5]}
  
  segments(x0=all_geo_res$richness[i],
           x1=all_geo_res$richness[i],
           y0=0,
           y1=all_geo_res$DeltaRank[i], col = coloo,lwd=2)
  
  
}

axis(2,at=c(seq(-1,1,by=.4)),
     labels=c("-1", "-0.6",  "-0.2",
              "0.2", "0.6", "1"),
     cex.axis=1,las=1)
axis(1,at=c(seq(0,3500,by=500)),
     labels=c("0", "500", "1000", "1500",
              "2000", "2500", "3000", "3500"),tick=TRUE,cex.axis=.8)
axis(1,at=c(seq(0,3500,by=500)),
     labels=FALSE,tick=FALSE,cex.axis=1)

dev.off()


#par(mar=c(3,6,0,2))










########################################################

################################TEST####################

########################################################



CellsIn = exactextractr::exact_extract(mask,mollBorder,include_xy=TRUE, include_cell = TRUE)

dim(CellsIn[[1]])

all_geo_res <- all_geo_res2

all_geo_res <- merge(all_geo_res,CellsIn, by.x = "ID",by.y = "cell" )


#all_geo_res  <- all_geo_res[sample(nrow(all_geo_res), 1000), ]
all_geo_res$richness <- all_geo_res$Rthr + all_geo_res$Rnothr + all_geo_res$Rnostatus
all_geo_res$DeltaRank <- all_geo_res$DeltaRank/max(abs(all_geo_res$DeltaRank))

pdf(file= here::here("figures","edgeplot.pdf"),height = 7.2, width = 11 ) 

layout(matrix(c(1,1,1,1,1,2),nrow=6,byrow=TRUE))
# the following line shows what this 2-panel layout looks like: 
# layout.show(2) 

par(mar=c(0,6,2,2)) 
plot(all_geo_res$y, # x-axis
     all_geo_res$DeltaRank, # y axis
     type="n",yaxt="n",ylim=c(-1,1),
     xlim=c(min(all_geo_res$y),max(all_geo_res$y)), # empty plot, sets axis limits.
     xaxt="n",ylab="",lty=1,
     main="Change in prioritization",
     pch=16,cex.main=1.4)

abline(h=0,col="darkgray")

text(x=3000,y=0.05,"positive",col="darkgray",cex=1.3) 
text(x=3000,y=-.05,"negative",col="darkgray",cex=1.3)

abline(v=0, col="lightgray") # gridlines
abline(v=500, col="lightgray") # gridlines
abline(v=1000, col="lightgray")
abline(v=1500, col="lightgray")
abline(v=2000, col="lightgray")
abline(v=2500, col="lightgray")
abline(v=3000, col="lightgray")

coloo <- NA
for (i in 1:nrow(all_geo_res)){
  
  if(all_geo_res$DeltaRank[i]< -0.5) {coloo=wes_palette(name="Zissou1")[1]}
  if(all_geo_res$DeltaRank[i]< 0 & all_geo_res$DeltaRank[i]> -0.5) {coloo=wes_palette(name="Zissou1")[2]}
  if(all_geo_res$DeltaRank[i]< 0.5 & all_geo_res$DeltaRank[i]> 0) {coloo=wes_palette(name="Zissou1")[3]}
  if(all_geo_res$DeltaRank[i] > 0.5) {coloo = wes_palette(name="Zissou1")[5]}
  
  segments(x0=all_geo_res$y[i],
           x1=all_geo_res$y[i],
           y0=0,
           y1=all_geo_res$DeltaRank[i], col = coloo,lwd=2)
  
  
}

axis(2,at=c(seq(-1,1,by=.4)),
     labels=c("-1", "-0.6",  "-0.2",
              "0.2", "0.6", "1"),
     cex.axis=1,las=1)
#axis(1,at=c(seq(min(all_geo_res$y),max(all_geo_res$y),by=5000000)),
 #    labels=c("-17675096", "-12675096",  "-7675096",  "-2675096", 
 #             "2324904",   "7324904",  "12324904",  "17324904"),tick=TRUE,cex.axis=.8)
#axis(1,at=c(seq(0,3500,by=500)),
#    labels=FALSE,tick=FALSE,cex.axis=1)

dev.off()


#par(mar=c(3,6,0,2))

dens <- density(all_geo_res$y)

plot(dens,main = " ", sub = NULL)
polygon(dens, col=wes_palette(name="Zissou1")[3], 
        border=wes_palette(name="Zissou1")[3],
        main = " ",sub = NULL) 

