# This R script contains the concatenated code from the day 3 tutorials.
# This is what you should work through during lab, using your own data.

library(geomorph)
library(geiger)

##### 1: Shape Statistics 1

## 1A: PCA, PACA, and phylo-PCA

data(plethspecies)
Y.gpa <- gpagen(plethspecies$land)

## PCA

PCA <- gm.prcomp(Y.gpa$coords, phy = plethspecies$phy,
                 align.to.phy = FALSE,
                 GLS = FALSE)
summary(PCA)
PCA$rot # loadings
PCAplot <- plot(PCA, pch = 16, phylo = TRUE)

# Explore shape change in the plot
picknplot.shape(PCAplot)
picknplot.shape(PCAplot, mag = 3)

# In case one wishes to use results for other reasons:
attributes(PCA)

# note that $d is the same as eigenvalues:

PCA$d

eigen(var(PCA$x), only.values = TRUE)$values

## PACA

PACA <- gm.prcomp(Y.gpa$coords, phy = plethspecies$phy,
                 align.to.phy = TRUE,
                 GLS = FALSE)
summary(PACA)
PACA$rot # loadings
PACAplot <- plot(PACA, pch = 16, phylo = TRUE)

# Explore shape change in the plot
picknplot.shape(PACAplot)
picknplot.shape(PACAplot, mag = 3)

## Phylo-PCA (discussed more with PCMs)


pPCA <- gm.prcomp(Y.gpa$coords, phy = plethspecies$phy,
                  align.to.phy = FALSE,
                  GLS = TRUE)
summary(pPCA)
pPCA$rot # loadings
pPCAplot <- plot(pPCA, pch = 16, phylo = TRUE)

# Explore shape change in the plot
picknplot.shape(pPCAplot)
picknplot.shape(pPCAplot, mag = 3)

## 1B: PLS

data("plethShapeFood")
Y.gpa <- gpagen(plethShapeFood$land)
food <- plethShapeFood$food
rownames(food) <- names(Y.gpa$Csize)

PLSfood <- two.b.pls(food, Y.gpa$coords, 
                     iter = 9999)
PLSallometry <- two.b.pls(Y.gpa$Csize, Y.gpa$coords,
                          iter = 9999)
PLSintegration <- two.b.pls(Y.gpa$coords[1:5,, ], 
                            Y.gpa$coords[8:13,,], 
                            iter = 9999)

summary(PLSfood)

# What results are provided?

attributes(PLSfood)

foodPlot <- plot(PLSfood, pch = 16)
picknplot.shape(foodPlot)

# If you want to get creative

hist(PLSfood$random.r, breaks = 50, 
     col = "olivedrab1", 
     main = "Sampling distribution of r")
abline(v = PLSfood$r.pls, lwd = 2, col = "skyblue4")

summary(PLSallometry)
allomPlot <- plot(PLSallometry, pch = 16)
picknplot.shape(allomPlot)

summary(PLSintegration)
integPlot <- plot(PLSintegration, pch = 16)
picknplot.shape(integPlot)

## PLS-comparison

compare.pls(PLSfood,
            PLSallometry,
            PLSintegration)

## 1C: GLM

fit.null <- procD.lm(coords ~ 1, data = Y.gpa,
                     iter = 9999)
fit.alt <- procD.lm(coords ~ log(Csize), data = Y.gpa,
                    iter = 9999)

model.matrix(fit.null)
coef(fit.null)
fitted(fit.null)
resid(fit.null)

model.matrix(fit.alt)
coef(fit.alt)
fitted(fit.alt)
resid(fit.alt)

## Hypothesis tests
coef(fit.alt, test = TRUE)

# uh-oh, we can fix this!
# The default is turbo = TRUE, which suppresses
# RRPP coefficients

fit.alt <- procD.lm(coords ~ log(Csize), data = Y.gpa,
                    turbo = FALSE,
                    iter = 9999,
                    Parallel = TRUE)

coef(fit.alt, test = TRUE)
anova(fit.null, fit.alt)
anova(fit.alt)

# Anything else worth exploring?

attributes(fit.alt)

##### 2: Allometry

## 2A: Simple (single-group) allometry

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

## 2B: Group allometry
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


##### 3: Shape Statistics II

data(pupfish) # GPA already performed

# Many different linear models

fit0 <- procD.lm(coords ~ 1, data = pupfish, iter = 999)
fit1 <- procD.lm(coords ~ log(CS), data = pupfish, iter = 999)
fit2 <- procD.lm(coords ~ Sex, data = pupfish, iter = 999)
fit3 <- procD.lm(coords ~ Pop, data = pupfish, iter = 999)
fit4 <- procD.lm(coords ~ log(CS) + Sex, data = pupfish, iter = 999)
fit5 <- procD.lm(coords ~ log(CS) + Pop, data = pupfish, iter = 999)
fit6 <- procD.lm(coords ~ log(CS) * Sex, data = pupfish, iter = 999)
fit7 <- procD.lm(coords ~ log(CS) * Pop, data = pupfish, iter = 999)
fit8 <- procD.lm(coords ~ Sex * Pop, data = pupfish, iter = 999)
fit9 <- procD.lm(coords ~ log(CS) + Sex*Pop, data = pupfish, iter = 999)
fit10 <- procD.lm(coords ~ log(CS) * Sex*Pop, data = pupfish, iter = 999)

# Model with just a mean

model.matrix(fit0)
coef(fit0)

# Linear regression model

model.matrix(fit1)
coef(fit1)

# Simple single-factor models

model.matrix(fit2)
coef(fit2)
model.matrix(fit3)
coef(fit3)

# How to find fitted values

unique(model.matrix(fit2)) %*% coef(fit2)[,1:4]
model.matrix(fit2) %*% coef(fit2)[,1:4]
fitted(fit2)[, 1:4]

# ANOVA

anova(fit8, effect.type = "Rsq")
anova(fit8, effect.type = "F")

# MANOVA

fitm <- manova.update(fit8)
summary(fitm)
summary(fitm, test = "Pillai")

# Pairwise comparisons, group means

group <- interaction(pupfish$Pop, pupfish$Sex)
PW <- pairwise(fit8, groups = group)
summary(PW)
summary(PW, stat.table = FALSE)

# You should explore the options with summary.pairwise

# Pairwise comparisons, group slopes

group <- interaction(pupfish$Pop, pupfish$Sex)
PW <- pairwise(fit10, groups = group, 
               covariate = log(pupfish$CS))
summary(PW)
summary(PW, test.type = "VC")
summary(PW, test.type = "VC", angle.type = "deg")

# Trajectory analysis (explored deeper in Day 4)

TA <- trajectory.analysis(fit8, group = pupfish$Pop,
                          traj.pts = pupfish$Sex)
summary(TA)
summary(TA, attribute = "TC")

TP <- plot(TA, pch = as.numeric(Pupfish$Pop) + 20, bg = as.numeric(Pupfish$Sex),
           cex = 0.7, col = "gray")
add.trajectories(TP, traj.pch = c(21, 22), start.bg = 1, end.bg = 2)
legend("topright", levels(Pupfish$Pop), pch =  c(21, 22), pt.bg = 1)

# Pairwise variance (disparity) analysis (explored deeper in Day 4)

PW <- pairwise(fit8, groups = group)
summary(PW, test.type = "var")

# Non-statistical model comparison

modComp1 <- model.comparison(fit1, fit2, fit3, fit4, fit5, 
                             fit6, fit7, fit8, fit9, fit10, type = "cov.trace")
modComp2 <- model.comparison(fit1, fit2, fit3, fit4, fit5, 
                             fit6, fit7, fit8, fit9, fit10, type = "logLik", tol = 0.01)

summary(modComp1)
summary(modComp2)

plot(modComp1)
plot(modComp2)
