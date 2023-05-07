# This R script contains the concatenated code from the day 4 tutorials.
# This is what you should work through during lab, using your own data.

library(geomorph)

##### 1: Phylogenetic Comparative Methods

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

### 2: Phylogenetic PLS
land.gps<-c("A","A","A","A","A","B","B","B","B","B","B")
PLS.Y <- phylo.integration(A = gdf$shape, partition.gp = land.gps, phy= plethtree, print.progress = FALSE)
summary(PLS.Y)
plot(PLS.Y)

### 3: Phylogenetic Signal
PS.shape <- physignal(A=shape,phy=plethtree,iter=999, print.progress = FALSE)
summary(PS.shape)
plot(PS.shape)

### 4: Phylogenetic Ordination

#### Phylomorphospace
plot.pca <- gm.prcomp(shape,phy=plethtree)
plot(plot.pca,phylo = TRUE, pch=21, bg=gdf$elev, cex=2, phylo.par = list(tip.labels = FALSE, node.labels = FALSE) )
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))

#### Phylogenetic PCA (pPCA)
plot.ppca <- gm.prcomp(shape,phy=plethtree, GLS = TRUE, transform = FALSE)
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

### 5: Comparing Evolutionary Rates

#### 5A: Comparing Rates Among Clades
ER<-compare.evol.rates(A=gdf$shape, phy=plethtree,gp=gdf$elev,iter=999, method = 'permutation',print.progress = FALSE)
summary(ER)
plot(ER)

#### 5B: Comparing Rates Among Traits
EMR <- compare.multi.evol.rates(A=gdf$shape, phy=plethtree, gp=c(rep(1,5),rep(2,6)), print.progress = FALSE)
summary(EMR)
plot(EMR)

## 6: Asymmetry

## matching symmetry
data(mosquito)
Y.gpa <- gpagen(mosquito$wingshape, print.progress = FALSE)
plot(Y.gpa)
mosquito.sym <- bilat.symmetry(A = Y.gpa, ind = mosquito$ind, side=mosquito$side, 
                               object.sym = FALSE, print.progress = FALSE)
summary(mosquito.sym)

## object symmetry
data('lizards')
Y.gpa <- gpagen(lizards$coords, print.progress = FALSE)
plot(Y.gpa)

lizard.sym <- bilat.symmetry(A = Y.gpa, ind = lizards$ind, replicate = lizards$rep,
                             object.sym = TRUE, land.pairs = lizards$lm.pairs, print.progress = FALSE)
summary(lizard.sym)
plot(lizard.sym, warpgrids = TRUE)

#### Comparison of overall vs. symmetrized aligned shapes
plotAllSpecimens(Y.gpa$coords)
plotAllSpecimens(lizard.sym$symm.shape)

## 7: Integration and Modularity
## Overall Integration
data("plethodon")
Y.gpa <- gpagen(plethodon$land, print.progress = FALSE)
#Separate data by species
coords.gp <- coords.subset(Y.gpa$coords, plethodon$species)

#Z_Vrel by species
Vrel.gp <- Map(function(x) integration.Vrel(x), coords.gp) 
compare.ZVrel(Vrel.gp$Jord, Vrel.gp$Teyah)

## Integration Across Spatial Scales
globalIntegration(Y.gpa$coords) #data are not spatially integrated

## Integration Among Subsets
data(pupfish) # GPA previously performed
group <- factor(paste(pupfish$Pop, pupfish$Sex, sep = "."))

# Subset 3D array by group, returning a list of 3D arrays
tail.LM <- c(1:3, 5:9, 18:38)
head.LM <- (1:56)[-tail.LM]
tail.coords <- pupfish$coords[tail.LM,,]
head.coords <- pupfish$coords[head.LM,,]

IT <- integration.test(tail.coords, head.coords, print.progress = F)
summary(IT)
plot(IT)

land.gp <- rep(1,56); land.gp[tail.LM] <- 2
integration.test(pupfish$coords, partition.gp=land.gp, print.progress = FALSE)

two.b.pls(tail.coords, head.coords, print.progress = FALSE)

## Comparing the Strength of Integration
tail.coords.gp <- coords.subset(tail.coords, group)
head.coords.gp <- coords.subset(head.coords, group)

# Obtain Integration for groups
integ.tests <- Map(function(x,y) integration.test(x, y, iter=499, 
                                                  print.progress = FALSE), head.coords.gp, tail.coords.gp)
compare.pls(integ.tests)

## Tests of Modularity
MT <- modularity.test(pupfish$coords,land.gp,CI=FALSE,print.progress = FALSE)
summary(MT)
plot(MT)

## Comparing the Strength of Modularity
coords.gp <- coords.subset(pupfish$coords, group)
modul.tests <- Map(function(x) modularity.test(x, land.gp,print.progress = FALSE), coords.gp) 
compare.CR(modul.tests, CR.null = FALSE)

## Comparing Alternative Modular Partitions
land.gps3 <- rep('a',56); land.gps3[39:48]<-'b'; land.gps3[c(6:9,28:38)] <- 'c' 
#3 module hypothesis (tail now a module)
land.gps4 <- rep('a',56); land.gps4[39:48]<-'b'; land.gps4[c(6:9,28:38)] <- 'c'; 
land.gps4[c(10,49:56)] <- 'd'  #4 module hypothesis (eye now a module)

m3.test <- modularity.test(coords.gp$Marsh.F,land.gps3, iter = 499, print.progress = FALSE)
m4.test <- modularity.test(coords.gp$Marsh.F,land.gps4, iter = 499, print.progress = FALSE)

model.Z <- compare.CR(m3.test,m4.test, CR.null = TRUE)
model.Z 
#########################################################

## 8: Trajectory analysis

# Factorial model approach

data(pupfish) # GPA already performed

TA <- trajectory.analysis(fit8, group = pupfish$Pop,
                          traj.pts = pupfish$Sex)
summary(TA)
summary(TA, attribute = "TC")

TP <- plot(TA, pch = as.numeric(Pupfish$Pop) + 20, bg = as.numeric(Pupfish$Sex),
           cex = 0.7, col = "gray")
add.trajectories(TP, traj.pch = c(21, 22), start.bg = 1, end.bg = 2)
legend("topright", levels(Pupfish$Pop), pch =  c(21, 22), pt.bg = 1)

# Trajectories as data (like motion paths)

data(motionpaths)
fit <- procD.lm(trajectories ~ groups, data = motionpaths, iter = 999, print.progress = FALSE)
anova(fit)
TA <- trajectory.analysis(fit, groups = motionpaths$groups, traj.pts = 5)
summary(TA, attribute = "MD") # Magnitude difference (absolute difference between path distances)
summary(TA, attribute = "TC", angle.type = "deg") # Correlations (angles) between trajectories
summary(TA, attribute = "SD") # Shape differences between trajectories 

TP <- plot(TA, pch = 21, bg = as.numeric(motionpaths$groups),
           cex = 0.7, col = "gray")
add.trajectories(TP, traj.pch = 21, traj.bg = 1:4)


## 9: Morphological Disparity

data(plethodon)
Y.gpa <- gpagen(plethodon$land, print.progress = FALSE)    
gdf <- geomorph.data.frame(Y.gpa, species = plethodon$species, site = plethodon$site)

# Typical group comparisons
MD <- morphol.disparity(coords ~ species*site, groups = ~species*site, 
                        data = gdf, iter = 999, print.progress = FALSE)
summary(MD)
gp <- interaction(plethodon$species, plethodon$site)
plotTangentSpace(Y.gpa$coords, groups=gp)

# Comparing species, despite species * site means estimation
MD2 <- morphol.disparity(coords ~ species*site, groups = ~species, 
                         data = gdf, iter = 999, print.progress = FALSE)
summary(MD2)

# Comparing group disparities from overall mean
MD3 <- morphol.disparity(coords ~ 1, groups = ~species, 
                         data = gdf, iter = 999, print.progress = FALSE)
summary(MD3)

# Comparing Foote's (1993) partial disparities
MD4 <- morphol.disparity(coords ~ 1, groups = ~species, partial = TRUE,
                         data = gdf, iter = 999, print.progress = FALSE)
summary(MD4)

### ----------------------------------------------------------------------------------
# Evaluating disparity in an allometric model

data(pupfish) # data already aligned
gdf <- geomorph.data.frame(coords = pupfish$coords, 
                           CS = pupfish$CS,
                           Pop = pupfish$Pop,
                           Sex = pupfish$Sex)
MD <- morphol.disparity(coords ~ Pop*Sex, groups = ~ Pop*Sex, 
                        data = gdf, iter = 999, print.progress = FALSE)
summary(MD)
gp <- interaction(pupfish$Pop, pupfish$Sex)
plotTangentSpace(pupfish$coords, groups=gp)

pupfish.allometry <- procD.lm(coords ~ log(CS) + Pop * Sex, data = gdf, 
                              iter = 999, print.progress = FALSE)
summary(pupfish.allometry)
MD2 <- morphol.disparity(pupfish.allometry, groups = gp, print.progress = FALSE)
summary(MD2)


