# This R script contains the concatenated code from the day 2 tutorials.
# This is what you should work through during lab, using your own data.


library(geomorph)

##### 1: GPA with semilandmarks

# Fixed points only
data(plethodon)
pleth.gpa <- gpagen(plethodon$land, print.progress = F)

plot(pleth.gpa)
plotAllSpecimens(pleth.gpa$coords, links = plethodon$links)

# Points and Curve points
data(hummingbirds)
hummingbirds$curvepts    
gpa.BE <- gpagen(hummingbirds$land, curves=hummingbirds$curvepts, ProcD=FALSE, print.progress = F)
plot(gpa.BE)

gpa.procD <- gpagen(hummingbirds$land, curves=hummingbirds$curvepts, ProcD=TRUE, print.progress = F)
plot(gpa.procD)

# StereoMorph Example
library(StereoMorph)

shapes <- readShapes("Data/example.digitized")
shapesGM <- readland.shapes(shapes, 
                            nCurvePts = c(12, 12, 12, 8, 6, 6, 6, 12, 10))

Y.gpa <- gpagen(shapesGM, print.progress = FALSE)
plot(Y.gpa)

# Points, Curves, and Surfaces
data(scallops)
scallops$surfslide  

#Using Procrustes Distance for sliding
gpa.scallop <- gpagen(A=scallops$coorddata, curves=scallops$curvslide, surfaces=scallops$surfslide, print.progress = F)
plot(gpa.scallop)

##### 2: PCA and Visualizing Shape Differences

# Plotting all specimens
data(plethodon)
Y.gpa <- gpagen(plethodon$land, print.progress = F)    # GPA-alignment

# PCA
PCA <- gm.prcomp(Y.gpa$coords)
summary(PCA)
plot(PCA)

# More Plotting Options
gps <- as.factor(paste(plethodon$species, plethodon$site))  #define some groups for plotting
plot(PCA, pch=22, cex = 1.5, bg = gps) 
legend("topleft", pch=22, pt.bg = unique(gps), legend = levels(gps))

## 2A: Shape Deformations Between Specimens

M <- mshape(Y.gpa$coords)

par(mfrow=c(1,2))
plotRefToTarget(M,Y.gpa$coords[,,39], links=plethodon$links)
mtext("TPS")
plotRefToTarget(M,Y.gpa$coords[,,39], mag=2.5, links=plethodon$links)
mtext("TPS: 2.5X magnification")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
plotRefToTarget(M,Y.gpa$coords[,,39], links=plethodon$links, method="vector", mag=3)
mtext("Vector Displacements")
plotRefToTarget(M,Y.gpa$coords[,,39], links=plethodon$links,gridPars=gridPar(pt.bg="red", link.col="green", pt.size = 1), method="vector", mag=3)
mtext("Vector Displacements: Other Options")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
plotRefToTarget(M,Y.gpa$coords[,,39], mag=2, outline=plethodon$outline)  
mtext("Outline Deformation")
plotRefToTarget(M,Y.gpa$coords[,,39], method="points", outline=plethodon$outline)
mtext("Outline Deformations Ref (gray) & and Tar (black)")
par(mfrow=c(1,1))

## 2B: Shape Predictions

# PCA-based
PC <- PCA$x[,1]
preds <- shape.predictor(Y.gpa$coords, x= PC, Intercept = FALSE, 
                         pred1 = min(PC), pred2 = max(PC)) # PC 1 extremes, more technically
par(mfrow=c(1,2))
plotRefToTarget(M, preds$pred1, links = plethodon$links)
mtext("PC1 - Min.")
plotRefToTarget(M, preds$pred2, links = plethodon$links)
mtext("PC1 - Max.")
par(mfrow=c(1,1))

# via picknplot.shape
pleth.pca.plot <- plot(PCA)
picknplot.shape(pleth.pca.plot) 

# Regression-based
gdf <- geomorph.data.frame(Y.gpa)
plethAllometry <- procD.lm(coords ~ log(Csize), data=gdf, print.progress = FALSE)
allom.plot <- plot(plethAllometry, 
                   type = "regression", 
                   predictor = log(gdf$Csize),
                   reg.type ="PredLine") # make sure to have a predictor 

preds <- shape.predictor(plethAllometry$GM$fitted, x= allom.plot$RegScore, Intercept = TRUE, 
                         predmin = min(allom.plot$RegScore), 
                         predmax = max(allom.plot$RegScore)) 
par(mfrow=c(1,2))
plotRefToTarget(M, preds$predmin, mag=3, links = plethodon$links)
mtext("Regression Min: 3X")
plotRefToTarget(M, preds$predmax, mag=3, links = plethodon$links)
mtext("Regression Max: 3X")
par(mfrow=c(1,1))

# via picknplot.shape (more detail below)
picknplot.shape(allom.plot) 

# Group Differences
gdf <- geomorph.data.frame(Y.gpa, species = plethodon$species, site = plethodon$site)
pleth.anova <- procD.lm(coords ~ species*site, data=gdf, print.progress = FALSE)
X <- pleth.anova$X
X # includes intercept; remove for better functioning 
X <- X[,-1]
symJord <- c(0,1,0) # design for P. Jordani in sympatry
alloJord <- c(0,0,0) # design for P. Jordani in allopatry
preds <- shape.predictor(arrayspecs(pleth.anova$fitted, 12, 2), x = X, Intercept = TRUE, 
                         symJord=symJord, alloJord=alloJord)
par(mfrow=c(1,2))
plotRefToTarget(M, preds$symJord, links = plethodon$links, mag=2)
mtext("Sympatric P. Jordani: 2X")
plotRefToTarget(M, preds$alloJord, links = plethodon$links, mag=2)
mtext("Allopatric P. Jordani: 2X")
par(mfrow=c(1,1))

# via picknplot.shape (more detail below)
plot.anova <- plot(pleth.anova, type = "PC", pch = 21, 
                   bg = interaction(gdf$species, gdf$site), 
                   asp = 1)
picknplot.shape(plot.anova) 

##### 2C: 3D Warping
scallops <- readland.tps("Data/scallops for viz.tps", specID = "ID")
ref <- mshape(scallops)
refmesh <- warpRefMesh(read.ply("Data/glyp02L.ply"), 
                       scallops[,,1], ref, color=NULL, centered=T)
PCA.scallop <- gm.prcomp(scallops)
plot(PCA.scallop, pch = 21, bg = "black", cex = 2)
PC.sc <- PCA.scallop$x[,1]
sc.preds <- shape.predictor(scallops, x= PC.sc, Intercept = FALSE, 
                         pred1 = min(PC.sc), pred2 = max(PC.sc)) # PC 1 extremes, more technically
plotRefToTarget(ref, sc.preds$pred1)
plotRefToTarget(ref, sc.preds$pred2)

plotRefToTarget(ref, sc.preds$pred1, mesh = refmesh, method = "surface", mag = 1)
plotRefToTarget(ref, sc.preds$pred2, mesh = refmesh, method = "surface", mag = 1)

