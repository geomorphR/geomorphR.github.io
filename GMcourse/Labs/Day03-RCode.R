# This R script contains the concatenated code from the day 3 tutorials.
# This is what you should work through during lab, using your own data.

library(geomorph)
library(geiger)

##### 1: Allometry

## 1A: Simple (single-group) allometry

data(pupfish)
plotAllSpecimens(pupfish$coords)  #NOTE: already GPA-aligned
   #Y.gpa <- gpagen(pupfish$coords, print.progress = FALSE)    #GPA-alignment  
pupfish$logSize <- log(pupfish$CS)
pupfish$Group <- interaction(pupfish$Pop, pupfish$Sex)

fit <- procD.lm(coords ~ logSize, data = pupfish, print.progress = FALSE) 
anova(fit)

 #plots
plot(fit, type = "regression", reg.type = "PredLine", predictor = pupfish$logSize, pch = 19)
plot(fit, type = "regression", reg.type = "RegScore", predictor = pupfish$logSize, pch = 19)
plotAllometry(fit, size = pupfish$logSize, logsz = FALSE, method = "CAC", pch = 19)

M <- mshape(pupfish$coords)
preds <- shape.predictor(fit$GM$fitted, x= pupfish$logSize, Intercept = TRUE, 
                         predmin = min(pupfish$logSize), 
                         predmax = max(pupfish$logSize)) 
par(mfrow=c(1,2))
plotRefToTarget(M, preds$predmin, mag=1)
mtext("Regression Min")
plotRefToTarget(M, preds$predmax, mag=1)
mtext("Regression Max")
par(mfrow=c(1,1))

## 1B: Group allometry
fit.common <- procD.lm(coords ~ logSize + Group, 
                       data = pupfish, print.progress = FALSE) 
fit.unique <- procD.lm(coords ~ logSize * Group, 
                       data = pupfish, print.progress = FALSE)  
anova(fit.unique)

### Pairwise Comparisons
slope.pw <- pairwise(fit.unique, fit.null = fit.common, 
                     groups = pupfish$Group,
                     covariate = pupfish$logSize, print.progress = FALSE)
summary(slope.pw, test.type = "VC", angle.type = "deg") # angular differences
summary(slope.pw, test.type = "dist", angle.type = "deg") # amount of shape change differences

par(mfcol = c(1,2))
plot(fit.common, type = "regression", predictor = pupfish$logSize, 
     reg.type = "PredLine", pch=19, col = pupfish$Group)
legend("topleft", levels(pupfish$Group), pch = 21, pt.bg = unique(pupfish$Group))
mtext("Common Slopes")
plot(fit.unique, type = "regression", predictor = pupfish$logSize, 
     reg.type = "PredLine", pch=19, col = pupfish$Group)
legend("topleft", levels(pupfish$Group), pch = 21, pt.bg = unique(pupfish$Group))
mtext("Unique Slopes")
par(mfcol = c(1,1))



## 1A: Simple (single-group) allometry


#######################################
##### 2: Shape Statistics II

#######################################

##### 3: Phylogenetic Comparative Methods

### Read and prune/match data
plethtree <- read.tree('Data/plethtree.tre')

plethtree <- read.tree('Data/plethtree.tre')
plethland <- readland.tps('Data/PlethodonLand.tps',specID = "ID",
                          warnmsg = FALSE)
gps <- read.csv('Data/PlethGps.csv', header=TRUE, row.names=1)
Y.gpa <- gpagen(plethland, print.progress = FALSE)
M <- mshape(Y.gpa$coords)
svl <- Y.gpa$Csize

shape <- Y.gpa$coords
shape.test <- treedata(phy = plethtree, data = two.d.array(shape), warnings = TRUE)
#no warnings. Everthing matches in this case

data.matched <- treedata(phy = plethtree, data = gps, warnings=FALSE)
elev <- as.factor(data.matched$data); names(elev) <- row.names(data.matched$data)

gdf <- geomorph.data.frame(shape=shape, svl=svl,elev = elev, plethtree=plethtree)

links <- matrix(c(4,3,2,1,1,6,7,8,9,10,1,1,11,5,5,4,2,3,7,8,9,10,11,9,10,1),
                ncol=2,byrow=FALSE)
plot(ladderize(plethtree),edge.width=3)
axisPhylo(1)

### 1A: Phylogenetic Regression
pgls.reg <- procD.pgls(f1 = shape~svl, phy=plethtree, data=gdf, print.progress = FALSE)
summary(pgls.reg)

#Plots
allom.plot <- plot(pgls.reg, type = "regression", predictor = gdf$svl,
                   reg.type ="RegScore", pch=19, cex=1.5, xlab = "SVL") # make sure to have a predictor 
fit.line <- lm(allom.plot$RegScore~gdf$svl)
abline(fit.line,col = "red")


preds <- shape.predictor(pgls.reg$GM$pgls.fitted, x= allom.plot$RegScore, Intercept = FALSE, 
                         predmin = min(allom.plot$RegScore), 
                         predmax = max(allom.plot$RegScore)) 
M <- mshape(shape)
par(mfrow = c(1,2))
plotRefToTarget(M, preds$predmin, mag=3, links = links)
mtext("Min")
plotRefToTarget(M, preds$predmax, mag=3, links = links)
mtext("Max")
par(mfrow = c(1,1))

### 1B: Phylogenetic ANOVA
pgls.aov <- procD.pgls(f1 = shape~elev, phy=plethtree, data=gdf, print.progress = FALSE)
summary(pgls.aov)

# Plots
plot.res <- gm.prcomp(shape,phy=plethtree)
plot(plot.res,phylo = FALSE, pch=21, bg=gdf$elev, cex=2)
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))

Low <- c(1) # design for low elevation
High <- c(0) # design for high elevation
preds <- shape.predictor(arrayspecs(pgls.aov$pgls.fitted, 11, 2), x = pgls.aov$X[,-1],
                         Intercept = TRUE, Low = Low, High = High)   
par(mfrow=c(1,2)) 
plotRefToTarget(M, preds$Low, mag=2, links=links)
mtext("Low Elevation")
plotRefToTarget(M, preds$High, mag=2, links=links)
mtext("High Elevation")
par(mfrow=c(1,1)) 

### 1C: Phylogenetic PLS
land.gps<-c("A","A","A","A","A","B","B","B","B","B","B")
PLS.Y <- phylo.integration(A = gdf$shape, partition.gp = land.gps, phy= plethtree, print.progress = FALSE)
summary(PLS.Y)
plot(PLS.Y)

### 2: Phylogenetic Signal
PS.shape <- physignal(A=shape,phy=plethtree,iter=999, print.progress = FALSE)
summary(PS.shape)
plot(PS.shape)

### 3: Phylogenetic Ordination

#### Phylomorphospace
plot.pca <- gm.prcomp(shape,phy=plethtree)
plot(plot.pca,phylo = TRUE, pch=21, bg=gdf$elev, cex=2, phylo.par = list(tip.labels = FALSE, node.labels = FALSE) )
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))

#### Phylogenetic PCA (pPCA)
plot.ppca <- gm.prcomp(shape,phy=plethtree, GLS = TRUE, transform = TRUE)
plot(plot.ppca,phylo = TRUE, pch=21, bg=gdf$elev, cex=2, phylo.par = list(tip.labels = FALSE, node.labels = FALSE) )
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))

#### Phylogenetically-Aligned Components Analysis (PACA)
plot.paca <- gm.prcomp(shape,phy=plethtree, align.to.phy = TRUE)
plot(plot.paca,phylo = TRUE, pch=21, bg=gdf$elev, cex=2, phylo.par = list(tip.labels = FALSE, node.labels = FALSE) )
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))

#### Side by Side
par(mfrow=c(1,3))
plot(plot.pca,phylo = TRUE, pch=21, bg=gdf$elev, cex=2, phylo.par = list(tip.labels = FALSE, node.labels = FALSE), main = "Phylomorphospace" )
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))

plot(plot.ppca,phylo = TRUE, pch=21, bg=gdf$elev, cex=2, phylo.par = list(tip.labels = FALSE, node.labels = FALSE), main = "pPCA" )
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))

plot(plot.paca,phylo = TRUE, pch=21, bg=gdf$elev, cex=2, phylo.par = list(tip.labels = FALSE, node.labels = FALSE), main = "PACA" )
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))
par(mfrow=c(1,1))

### 4: Comparing Evolutionary Rates

#### 4A: Comparing Rates Among Clades
ER<-compare.evol.rates(A=gdf$shape, phy=plethtree,gp=gdf$elev,iter=999, method = 'permutation',print.progress = FALSE)
summary(ER)
plot(ER)

#### 4B: Comparing Rates Among Traits
EMR <- compare.multi.evol.rates(A=gdf$shape, phy=plethtree, gp=c(rep(1,5),rep(2,6)), print.progress = FALSE)
summary(EMR)
plot(EMR)
