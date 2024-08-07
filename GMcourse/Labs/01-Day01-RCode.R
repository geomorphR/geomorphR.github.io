## Day 1 Material R-script

library(geomorph)

# Reading Data
mydata <- readland.tps("Data/salamanders.tps", specID="imageID") # Specify specimen labels
dim(mydata)
mydata[,,1]

# GPA
data(plethodon)
Y.gpa <- gpagen(plethodon$land, print.progress = F)

plotAllSpecimens(plethodon$land, links = plethodon$links)
plotAllSpecimens(Y.gpa$coords, links = plethodon$links)

## Check outliers
plotOutliers(Y.gpa$coords, inspect.outliers = T)

# PCA
PCA <- gm.prcomp(Y.gpa$coords)
plot(PCA)

gps <- as.factor(paste(plethodon$species, plethodon$site))  #define some groups for plotting
plot(PCA, pch=22, cex = 1.5, bg = gps) 
legend("topleft", pch=22, pt.bg = unique(gps), legend = unique(gps))

# TPS
M <- mshape(Y.gpa$coords)

par(mfrow=c(1,2))
plotRefToTarget(M,Y.gpa$coords[,,39], links=plethodon$links, method="vector", mag=3)
mtext("Vector Displacements")
plotRefToTarget(M,Y.gpa$coords[,,39], links=plethodon$links, 
                gridPars=gridPar(pt.bg="red", link.col="green", pt.size = 1), method="vector", mag=3)
mtext("Vector Displacements: Other Options")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
plotRefToTarget(M,Y.gpa$coords[,,39], mag=2, outline=plethodon$outline)  
mtext("Outline Deformation")
plotRefToTarget(M,Y.gpa$coords[,,39], method="points", outline=plethodon$outline)
mtext("Outline Deformations Ref (gray) & and Tar (black)")
par(mfrow=c(1,1))

# Shape Prediction (PCA)

PC <- PCA$x[,1]
preds <- shape.predictor(Y.gpa$coords, x= PC, Intercept = FALSE, 
                         pred1 = min(PC), pred2 = max(PC)) # PC 1 extremes, more technically
par(mfrow=c(1,2))
plotRefToTarget(M, preds$pred1, links = plethodon$links)
mtext("PC1 - Min.")
plotRefToTarget(M, preds$pred2, links = plethodon$links)
mtext("PC1 - Max.")
par(mfrow=c(1,1))

