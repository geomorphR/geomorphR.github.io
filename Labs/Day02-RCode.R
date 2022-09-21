# This R script contains the concatenatid code from the day 2 tutorials.
# This is what you should work through during lab, using your own data.


library(geomorph)

##### 1: GPA with semilandmarks

# Fixed points only
data(plethodon)
pleth.gpa <- gpagen(plethodon$land, print.progress = F)
summary(pleth.gpa)

plot(pleth.gpa)
plotAllSpecimens(pleth.gpa$coords, links = plethodon$links)

# Points and Curve points
data(hummingbirds)
hummingbirds$curvepts   
gpa.BE <- gpagen(hummingbirds$land, curves=hummingbirds$curvepts, ProcD=FALSE, print.progress = F)
plot(gpa.BE)

gpa.procD <- gpagen(hummingbirds$land, curves=hummingbirds$curvepts, ProcD=TRUE, print.progress = F)
plot(gpa.procD)

# Points, Curves, and Surfaces
data(scallops)
scallops$surfslide  
gpa.scallop <- gpagen(A=scallops$coorddata, curves=scallops$curvslide, surfaces=scallops$surfslide, print.progress = F)
plot(gpa.scallop)

# Points and curves via readland.shapes

library(StereoMorph)

shapes <- readShapes("example.digitized")
shapesGM <- readland.shapes(shapes, 
                            nCurvePts = c(12, 12, 12, 8, 6, 6, 6, 12, 10))

shapesGM$curves
gpa.pupfish <- gpagen(shapesGM)
plot(gpa.pupfish)

##### 2: Visualizing Shape Differences and PCA

# Plotting all specimens
data(plethodon)
Y.gpa <- gpagen(plethodon$land, print.progress = F)    # GPA-alignment

par(mfrow=c(1,2)) 
plotAllSpecimens(plethodon$land, links=plethodon$links)  # Raw data
mtext("Raw Data")
plotAllSpecimens(Y.gpa$coords, links=plethodon$links)    # GPA-aligned data
mtext("GPA-Aligned Specimens")
par(mfrow=c(1,1)) 

# Types of deformations

ref <- mshape(Y.gpa$coords)
par(mfrow=c(3,2))
plotRefToTarget(ref,Y.gpa$coords[,,39], links=plethodon$links)
mtext("TPS")
plotRefToTarget(ref,Y.gpa$coords[,,39],mag=2.5, links=plethodon$links)
mtext("TPS: 2.5X magnification")

plotRefToTarget(ref,Y.gpa$coords[,,39], links=plethodon$links,method="vector",mag=3)
mtext("Vector Displacements")
plotRefToTarget(ref,Y.gpa$coords[,,39], links=plethodon$links,gridPars=gridPar(pt.bg="red", link.col="green", pt.size = 1),
                method="vector",mag=3)
mtext("Vector Displacements: Other Options")

plotRefToTarget(ref,Y.gpa$coords[,,39],mag=2,outline=plethodon$outline)  
mtext("Outline Deformation")
plotRefToTarget(ref,Y.gpa$coords[,,39],method="points",outline=plethodon$outline)
mtext("OUtline Deformations Ref (gray) & and Tar (black)")
par(mfrow=c(1,1))

# Shape Predictions

# PCA-based
M <- mshape(Y.gpa$coords)
PCA <- plotTangentSpace(Y.gpa$coords)
PC <- PCA$pc.scores[,1]
preds <- shape.predictor(Y.gpa$coords, x= PC, Intercept = FALSE, 
                         pred1 = min(PC), pred2 = max(PC)) # PC 1 extremes, more technically
plotRefToTarget(M, preds$pred1, links = plethodon$links)
mtext("PC1 - Min.")
plotRefToTarget(M, preds$pred2, links = plethodon$links)
mtext("PC1 - Max.")

# Regression-based
gdf <- geomorph.data.frame(Y.gpa)
plethAllometry <- procD.lm(coords ~ log(Csize), data=gdf, print.progress = FALSE)
allom.plot <- plot(plethAllometry, 
                   type = "regression", 
                   predictor = log(gdf$Csize),
                   reg.type ="PredLine") # make sure to have a predictor 

preds <- shape.predictor(plethAllometry$GM$fitted, x= allom.plot$RegScore, Intercept = FALSE, 
                         predmin = min(allom.plot$RegScore), 
                         predmax = max(allom.plot$RegScore)) 
plotRefToTarget(M, preds$predmin, mag=3, links = plethodon$links)
plotRefToTarget(M, preds$predmax, mag=3, links = plethodon$links)

# via picknplot.shape (more detail below)

picknplot.shape(allom.plot) 

# Group difference-based
gdf <- geomorph.data.frame(Y.gpa, species = plethodon$species, site = plethodon$site)
pleth.anova <- procD.lm(coords ~ species*site, data=gdf, print.progress = FALSE)
X <- pleth.anova$X
X # includes intercept; remove for better functioning 
X <- X[,-1]
symJord <- c(0,1,0) # design for P. Jordani in sympatry
alloJord <- c(0,0,0) # design for P. Jordani in allopatry
preds <- shape.predictor(pleth.anova$GM$fitted, x = X, Intercept = TRUE, 
                         symJord=symJord, alloJord=alloJord)
plotRefToTarget(M, preds$symJord, links = plethodon$links, mag=2)
plotRefToTarget(M, preds$alloJord, links = plethodon$links, mag=2)

# via picknplot.shape (more detail below)

plot.anova <- plot(pleth.anova, type = "PC", pch = 21, 
                   bg = interaction(gdf$species, gdf$site), 
                   asp = 1)

picknplot.shape(plot.anova) 

##### 3: Principal Components Analysis (PCA)

plotTangentSpace(Y.gpa$coords, groups = interaction(plethodon$species, plethodon$site))

pleth.raw <- gm.prcomp(Y.gpa$coords)

gps <- as.factor(paste(plethodon$species, plethodon$site))
plot(pleth.raw)
par(mar=c(2, 2, 2, 2))
plot(pleth.raw, pch=22, cex = 1.5, bg = gps) 
#  Add things as desired using standard R plotting
text(par()$usr[1], 0.1*par()$usr[3], labels = "PC1 - 45.64%", pos = 4, font = 2)
text(0, 0.95*par()$usr[4], labels = "PC2 - 18.80%", pos = 4, font = 2)
legend("topleft", pch=22, pt.bg = unique(gps), legend = levels(gps))

##### 4: PickNPlot Shapes in Real Time (more detail here)

data(plethodon) 
Y.gpa <- gpagen(plethodon$land)
pleth.pca <- gm.prcomp(Y.gpa$coords)

pleth.pca.plot <- plot(pleth.pca)
picknplot.shape(pleth.pca.plot) 

picknplot.shape(plot(pleth.pca), method = "points", mag = 3, links=plethodon$links)

##### 5: 3D Warping

scallops <- readland.tps("Data/scallops for viz.tps", specID = "ID")
ref <- mshape(scallops)
refmesh <- warpRefMesh(read.ply("Data/glyp02L.ply"), 
                       scallops[,,1], ref, color=NULL, centered=T)
plotTangentSpace(scallops, axis1 = 1, axis2 = 2, warpgrids=T, mesh= refmesh)

##### 6: Two-Block Partial Least Squares (PLS)

data(pupfish)
Y.gpa <- gpagen(pupfish$coords, print.progress = FALSE)
plotAllSpecimens(Y.gpa$coords)
shape <- Y.gpa$coords
headland <- c(4, 10:17, 39:56)

PLS <- two.b.pls(shape[headland,,],shape[-headland,,], iter=999, print.progress = FALSE)
summary(PLS)
pls.plot <- plot(PLS)

## PLS shape predictions
preds <- shape.predictor(shape[headland,,], two.d.array(shape[-headland,,]), Intercept = FALSE,
                         method = "PLS", pred1 = -0.2, pred2 = 0.2) # using PLS plot as a guide

M <- mshape(shape[headland,,])
plotRefToTarget(M, -1*preds$pred1, mag=3)
plotRefToTarget(M, preds$pred2, mag=3)

# via picknplot.shape (more detail above)

picknplot.shape(pls.plot, mag = 3) 

##### 7: Regression

pupfish$logSize <- log(pupfish$CS)  #add logCS to geomorph data frame
fit <- procD.lm(coords ~ logSize, data = pupfish, print.progress = FALSE)
anova(fit)

plot(fit)
plot(fit, type = "regression", reg.type = "PredLine", predictor = pupfish$logSize, pch=21, bg="red")
plot(fit, type = "regression", reg.type = "RegScore", predictor = pupfish$logSize, pch=21, bg="red")

## Regression predictions
allom.plot <- plot(fit, 
                   type = "regression", 
                   predictor = pupfish$logSize,
                   reg.type ="RegScore") # make sure to have a predictor 

preds <- shape.predictor(allom.plot$GM$fitted, x= allom.plot$RegScore, Intercept = FALSE, 
                         predmin = min(allom.plot$RegScore), 
                         predmax = max(allom.plot$RegScore)) 
M <- mshape(pupfish$coords)
plotRefToTarget(M, preds$predmin, mag=3)
plotRefToTarget(M, preds$predmax, mag=3)


# via picknplot.shape (more detail above)

picknplot.shape(allom.plot, mag = 3) 

