## Example data from Adams & Collyer 2015

library(ape)
library(geomorph)
plethtree <- read.tree('plethtree.tre')
dat <- read.csv('svl.csv', header=TRUE, row.names=1)
svl <-dat[,1]; names(svl) <- rownames(dat)
shape <- readland.tps('headshape.tps',specID = "ID",warnmsg = FALSE)
plethgps <- read.csv('Gps.csv',header=TRUE, row.names=1)
  plethgps <- plethgps[match(dimnames(shape)[[3]],rownames(plethgps)),]

  elev <- as.factor(plethgps$ElevGp); names(elev) <- rownames(plethgps)
  
  gdf <- geomorph.data.frame(shape=shape, svl=svl,elev = elev, plethtree=plethtree)
links <- matrix(c(4,3,2,1,1,6,7,8,9,10,1,1,11,5,5,4,2,3,7,8,9,10,11,9,10,1),
                ncol=2,byrow=FALSE)
plot(ladderize(plethtree),edge.width=3)
axisPhylo(1)

plotAllSpecimens(shape,links=links)

##Regression
pgls.reg <- procD.pgls(f1 = shape~svl, effect.type = 'cohen', data=gdf,
                       phy=plethtree, print.progress = FALSE)
summary(pgls.reg)$table

plot.res <- gm.prcomp(shape,phy=plethtree, data=gdf)
plot(plot.res,phylo = TRUE, pch=21, bg="red", cex=1.5)

## anova
pgls.aov <- procD.pgls(f1 = shape~elev, effect.type = 'cohen', data=gdf,
                       phy=plethtree, print.progress = FALSE)
summary(pgls.aov)$table

pc.plot <- plot(pgls.aov,type = "PC", pch=21, cex=1.5,bg=gdf$elev)

shapeHulls(pc.plot, groups = gdf$elev, 
           group.cols = c("red", "black"),
           group.lwd = rep(1, 2), group.lty = c(2, 1))
legend("topright", levels(gdf$elev), 
       col = c("black", "red"),
       lwd = rep(1,2), lty = c(2, 1))

M <- mshape(shape)
Low <- c(1) # design for low elevation
High <- c(0) # design for high elevation
preds <- shape.predictor(arrayspecs(pgls.aov$pgls.fitted, 11,2), x= pgls.aov$X[,-1],
           Intercept = TRUE, Low = Low, High = High)                   
plotRefToTarget(M, preds$Low, mag=2, links=links)
plotRefToTarget(M, preds$High, mag=2, links=links)

land.gps<-c("A","A","A","A","A","B","B","B","B","B","B")
PLS.Y <- phylo.integration(A = gdf$shape, partition.gp = land.gps, phy= plethtree, print.progress = FALSE)
PLS.Y$r.pls; PLS.Y$P.value
plot(PLS.Y)

ER<-compare.evol.rates(A=gdf$shape, phy=plethtree,gp=gdf$elev,iter=999, method = 'permutation',print.progress = FALSE)
summary(ER)
plot(ER)

EMR <- compare.multi.evol.rates(A=gdf$shape, phy=plethtree, gp=land.gps, print.progress = FALSE)
summary(EMR)
plot(EMR)