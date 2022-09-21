# This R script contains the concatenatid code from the day 3 tutorials.
# This is what you should work through during lab, using your own data.

library(geomorph)

##### 1: ANOVA and pairwise comparisons ==============================================================

data(plethodon) # SOME EXAMPLE DATA
Y.gpa <- gpagen(plethodon$land, print.progress = FALSE)  
plot(Y.gpa)

gdf <- geomorph.data.frame(Y.gpa, 
                           site = plethodon$site, 
                           species = plethodon$species,
                           gp = interaction(plethodon$species, plethodon$site)) # geomorph data frame
# Single-Factor ANOVA
pleth.anova <- procD.lm(coords ~ species, data = gdf, print.progress = FALSE)
anova(pleth.anova)
PCA <- plotTangentSpace(Y.gpa$coords, groups = gdf$species) 

# Before going further, let's look at some of the attributes of procD.lm object.
# This will help to understand how a lot of downstream analyses work.

attributes(pleth.anova)

# The objects, $call, $LM, $ANOVA, and $PermInfo are generated from the lm.rrpp function in the 
# RRPP package.  The preceding objects are all found in these RRPP objects, or calculated from them,
# and are retained this way for historical consideration 
# (they were generated before procD.lm depended on RRPP).
# The $GM object is a new feature (since geomorph version 3.1.0).  
# When data are GM data (landmarks in 3D arrays),
# the $GM object will arrange fitted values, residuals, and coefficients into the 3D format 
# for shape prediction.  For example:

pleth.anova$GM$fitted[,, 1:4]
pleth.anova$GM$residuals[,, 1:4]
pleth.anova$GM$coefficients # effects

# Let's see the species effect as a TPS plot 

ref <- mshape(Y.gpa$coords)

par(mfcol = c(1, 2))

# P. jordani (just the intercept; i.e., 1, 0)
plotRefToTarget(ref,  pleth.anova$GM$coefficients[,, 1], mag = 3)
mtext("P. jordani")

# P. teyahelee (intercept + slope; i.e., 1, 1)
plotRefToTarget(ref,  pleth.anova$GM$coefficients[,, 1] + pleth.anova$GM$coefficients[,, 2], mag = 3)
mtext("P. teyahalee")
par(mfcol = c(1,1))

### --------------------------------------------------------------------------------------------------

# MANOVA statistics

pleth.manova <- manova.update(pleth.anova, tol = 0)
summary(pleth.manova)
summary(pleth.manova, test = "Pillai")
summary(pleth.manova, test = "Wilks")


### --------------------------------------------------------------------------------------------------

# Factorial Models with Pairwise Comparisons
pleth.anova2 <- procD.lm(coords ~ species*site, data = gdf, print.progress = FALSE)
anova(pleth.anova2)

# Before performing pairwise comparisons, it might be useful to see what the null model is
reveal.model.designs(pleth.anova2)

pleth.pw <- pairwise(pleth.anova2, groups = gdf$gp)
summary(pleth.pw, confidence = 0.95, test.type = "dist")

# We could override the null model, if we wanted

pleth.null <- procD.lm(coords ~ 1, data = gdf, print.progress = FALSE)
pleth.pw2 <- pairwise(pleth.anova2, fit.null = pleth.null, groups = gdf$gp)
summary(pleth.pw2, confidence = 0.95, test.type = "dist")

# Note that the former paiwise test considered whether means were different, given species 
# and site differences.  The latter pairwise test considered whether means were different, 
# given the overall mean was sufficient as a null model.

### --------------------------------------------------------------------------------------------------

## plots
pleth.raw <- gm.prcomp(Y.gpa$coords)
gps <- as.factor(paste(plethodon$species, plethodon$site))
par(mar=c(2, 2, 2, 2))
plot(pleth.raw, pch=22, cex = 1.5, bg = gps) 
#  Add things as desired using standard R plotting
text(par()$usr[1], 0.1*par()$usr[3], labels = "PC1 - 45.64%", pos = 4, font = 2)
text(0, 0.95*par()$usr[4], labels = "PC2 - 18.80%", pos = 4, font = 2)
legend("topleft", pch=22, pt.bg = unique(gps), legend = levels(gps))

M <- mshape(Y.gpa$coords)
pleth.anova <- procD.lm(coords ~ species*site, data=gdf, print.progress = FALSE)
X <- pleth.anova$X
X[1:10,] # includes intercept; remove for better functioning 
X <- X[,-1]
symJord <- c(0,1,0) # design for P. Jordani in sympatry
alloJord <- c(0,0,0) # design for P. Jordani in allopatry
preds <- shape.predictor(pleth.anova$GM$fitted, x = X, Intercept = TRUE, 
                         symJord=symJord, alloJord=alloJord)
plotRefToTarget(M, preds$symJord, links = plethodon$links, mag=2)
plotRefToTarget(M, preds$alloJord, links = plethodon$links, mag=2)

# via picknplot
par(mar = c(5, 5, 2, 2))
pleth.anova.plot <- plot(pleth.anova, type = "PC", pch = 21, 
                         bg = interaction(gdf$species, gdf$site))
picknplot.shape(pleth.anova.plot)

### --------------------------------------------------------------------------------------------------

## Nested Models (i.e., error term adjustment)
data("larvalMorph")
Y.gpa <- gpagen(larvalMorph$tailcoords, curves = larvalMorph$tail.sliders,
                ProcD = TRUE, print.progress = FALSE)
gdf <- geomorph.data.frame(Y.gpa, treatment = larvalMorph$treatment, 
                           family = larvalMorph$family)

fit <- procD.lm(coords ~ treatment/family, data = gdf, 
                print.progress = FALSE, iter = 199)
anova(fit) # treatment effect not adjusted
anova(fit, error = c("treatment:family", "Residuals")) # treatment effect updated (adjusted)

##### 2: Trajectory Analysis =========================================================================

## Estimated via Linear Models
data(pupfish)
fit <- procD.lm(coords ~ Pop * Sex, data = pupfish, iter = 999, print.progress = FALSE)
reveal.model.designs(fit)
anova(fit)
TA <- trajectory.analysis(fit, groups = pupfish$Pop, traj.pts = pupfish$Sex)
summary(TA, attribute = "MD") # Magnitude difference (absolute difference between path distances)
summary(TA, attribute = "TC", angle.type = "deg") # Correlations (angles) between trajectories
summary(TA, attribute = "SD") # No shape differences between vectors

# Retain results
TA.summary <- summary(TA, attribute = "MD")
TA.summary$summary.table

# Alternate summary style
summary(TA, attribute = "MD", stat.table = FALSE)

# Plot results
TP <- plot(TA, pch = as.numeric(pupfish$Pop) + 20, bg = as.numeric(pupfish$Sex),
           cex = 0.7, col = "gray")
add.trajectories(TP, traj.pch = c(21, 22), start.bg = 1, end.bg = 2)
legend("topright", levels(pupfish$Pop), pch =  c(21, 22), pt.bg = 1)

### --------------------------------------------------------------------------------------------------

## Input as Trajectories
data(motionpaths)
fit <- lm.rrpp(trajectories ~ groups, data = motionpaths, iter = 999, print.progress = FALSE)
anova(fit)
TA <- trajectory.analysis(fit, groups = motionpaths$groups, traj.pts = 5)
summary(TA, attribute = "MD") # Magnitude difference (absolute difference between path distances)
summary(TA, attribute = "TC", angle.type = "deg") # Correlations (angles) between trajectories
summary(TA, attribute = "SD") # Shape differences between trajectories 

TP <- plot(TA, pch = 21, bg = as.numeric(motionpaths$groups),
           cex = 0.7, col = "gray")
add.trajectories(TP, traj.pch = 21, traj.bg = 1:4)


##### 3: Phylogenetic Comparative Methods ============================================================

# data input
library(geiger)
plethtree <- read.tree('Data/plethtree.tre')
plot(plethtree)

dat <- read.csv('Data/svl.csv', header=TRUE, row.names=1)
svl <-dat[,1]; names(svl) <- rownames(dat)
shape <- readland.tps('Data/headshape.tps',specID = "ID",warnmsg = FALSE)

match.data <- treedata(plethtree,svl)  

plethgps <- read.csv('Data/Gps.csv',header=TRUE, row.names=1)
plethgps <- plethgps[match(dimnames(shape)[[3]],rownames(plethgps)),]
elev <- as.factor(plethgps$ElevGp); names(elev) <- rownames(plethgps)

gdf <- geomorph.data.frame(shape=shape, svl=svl,elev = elev, plethtree=plethtree)
links <- matrix(c(4,3,2,1,1,6,7,8,9,10,1,1,11,5,5,4,2,3,7,8,9,10,11,9,10,1),
                ncol=2,byrow=FALSE)
plot(ladderize(plethtree),edge.width=3)
axisPhylo(1)

### --------------------------------------------------------------------------------------------------

# PGLS regression
pgls.reg <- procD.pgls(f1 = shape~svl, effect.type = 'cohen', data=gdf,
                       phy=plethtree, print.progress = FALSE)
summary(pgls.reg)
allom.plot <- plot(pgls.reg, type = "regression", predictor = gdf$svl,
                   reg.type ="RegScore", pch=19, cex=1.5) # make sure to have a predictor 

fit.line <- lm(allom.plot$RegScore~gdf$svl)
abline(fit.line,col = "red")
#plots
preds <- shape.predictor(pgls.reg$GM$pgls.fitted, x= allom.plot$RegScore, Intercept = FALSE, 
                         predmin = min(allom.plot$RegScore), 
                         predmax = max(allom.plot$RegScore)) 
M <- mshape(shape)
plotRefToTarget(M, preds$predmin, mag=3, links = links)
plotRefToTarget(M, preds$predmax, mag=3, links = links)

### --------------------------------------------------------------------------------------------------

# PGLS ANOVA
pgls.aov <- procD.pgls(f1 = shape~elev, effect.type = 'cohen', data=gdf,
                       phy=plethtree, print.progress = FALSE)
summary(pgls.aov)

plot.res <- gm.prcomp(shape,phy=plethtree, data=gdf)
ord.plot <- plot(plot.res,phylo = FALSE, pch=21, bg=gdf$elev, cex=1.5)
shapeHulls(ord.plot, groups = gdf$elev, 
           group.cols = c("red", "black"),
           group.lwd = rep(1, 2), group.lty = c(2, 1))
legend("topright", levels(gdf$elev), 
       col = c("black", "red"),
       lwd = rep(1,2), lty = c(2, 1))

# plots
Low <- c(1) # design for low elevation
High <- c(0) # design for high elevation
preds <- shape.predictor(arrayspecs(pgls.aov$pgls.fitted, 11,2), x= pgls.aov$X[,-1],
                         Intercept = TRUE, Low = Low, High = High)   
par(mfrow=c(1,2)) 
plotRefToTarget(M, preds$Low, mag=2, links=links)
mtext("Low Elevation")
plotRefToTarget(M, preds$High, mag=2, links=links)
mtext("High Elevation")
par(mfrow=c(1,1)) 

# via picknplot.shape
picknplot.shape(pc.plot)

### --------------------------------------------------------------------------------------------------

# Phylogenetic PLS
land.gps<-c("A","A","A","A","A","B","B","B","B","B","B")
PLS.Y <- phylo.integration(A = gdf$shape, partition.gp = land.gps, 
                           phy= plethtree, print.progress = FALSE)
summary(PLS.Y)
pls.plot <- plot(PLS.Y)
picknplot.shape(pls.plot, mag = 2)

# Phylogenetic Ordination
plot.res <- gm.prcomp(shape,phy=plethtree, data=gdf)
ord.plot <- plot(plot.res,phylo = TRUE, pch=21, bg="red", cex=1.5)
picknplot.shape(ord.plot, mag = 2)

# Phylogenetic Signal
PS.shape <- physignal(gdf$shape, gdf$plethtree, print.progress = FALSE)
summary(PS.shape)
plot(PS.shape)

# Comparing Net Rates of Evolution
ER<-compare.evol.rates(A=gdf$shape, phy=plethtree,gp=gdf$elev,iter=999, 
                       method = 'permutation', print.progress = FALSE)
summary(ER)
plot(ER)  # COMPARISONS AMONG CLADES

EMR <- compare.multi.evol.rates(A=gdf$shape, phy=plethtree, gp=land.gps, print.progress = FALSE)
summary(EMR)
plot(EMR)  # COMPARISONS AMONG TRAITS

##### 4: Allometry ===================================================================================

# Simple Allometry
data(plethodon) 
Y.gpa <- gpagen(plethodon$land, print.progress = FALSE)    #GPA-alignment  
gdf <- geomorph.data.frame(Y.gpa, site = plethodon$site, 
                           species = plethodon$species) 
fit <- procD.lm(coords ~ log(Csize), data=gdf, iter=999, print.progress = FALSE)
anova(fit)

# Predline
plotAllometry(fit, size = gdf$Csize, logsz = TRUE, method = "PredLine", pch = 19)

# RegScore
plotAllometry(fit, size = gdf$Csize, logsz = TRUE, method = "RegScore", pch = 19)

# CAC
plotAllometry(fit, size = gdf$Csize, logsz = TRUE, method = "CAC", pch = 19)

### --------------------------------------------------------------------------------------------------

# Group Allometry, including homogeneity of slopes test

fit.unique <- procD.lm(coords ~ Csize * species * site, data=gdf, iter=999, print.progress = FALSE)
fit.common <- procD.lm(coords ~ Csize + species * site, data=gdf, iter=999, print.progress = FALSE)
anova(fit.common, fit.unique)

# Because the unique slopes model was slightly better, it seems unwise to assume slopes
# are parallel and compare means.  However, the additional explained variation with unique slopes 
# was also quite small.
# Let's see what happens when we compare slopes.
# We can compare slopes with pairwise, just like means.
# Let's make sure the common slopes model is the null model.

### --------------------------------------------------------------------------------------------------

# Pairwise comparisons
slope.pw <- pairwise(fit.unique, fit.null = fit.common, 
                     groups = interaction(gdf$species, gdf$site),
                     covariate = gdf$Csize)
summary(slope.pw, test.type = "VC", angle.type = "deg") # angular differences
summary(slope.pw, test.type = "dist", angle.type = "deg") # amount of shape change differences

# Conclusion: some slight differences in angles between slopes
# Note that the UCL angles are quite large - usually an indication of 
# small size ranges.

### --------------------------------------------------------------------------------------------------
# Plots

# Predline
plotAllometry(fit.unique, size = gdf$Csize, logsz = TRUE, method = "PredLine", 
              pch = 19, col = as.numeric(interaction(gdf$species, gdf$site)))

# RegScore
plotAllometry(fit.unique, size = gdf$Csize, logsz = TRUE, method = "RegScore", 
              pch = 19, col = as.numeric(interaction(gdf$species, gdf$site)))


# Size-Shape Space
pc.plot <- plotAllometry(fit.unique, size = gdf$Csize, logsz = TRUE, method = "size.shape", 
                         pch = 19, col = as.numeric(interaction(gdf$species, gdf$site)))
summary(pc.plot$size.shape.PCA)

# with picknplot.shape

picknplot.shape(pc.plot)

### --------------------------------------------------------------------------------------------------
