## Day 2 Material R-script

library(geomorph)

# GPA with Sliders

data(hummingbirds)
hummingbirds$curvepts   

gpa.BE <- gpagen(hummingbirds$land, curves=hummingbirds$curvepts, ProcD=FALSE, print.progress = F)
plot(gpa.BE)

gpa.procD <- gpagen(hummingbirds$land, curves=hummingbirds$curvepts, ProcD=TRUE, print.progress = F)
plot(gpa.procD)

#### StereoMorph Example
shapes <- StereoMorph::readShapes("Data/example.digitized")
shapesGM <- readland.shapes(shapes, 
                            nCurvePts = c(12, 12, 12, 8, 6, 6, 6, 12, 10))

Y.gpa <- gpagen(shapesGM, print.progress = FALSE)
plot(Y.gpa)

## Partial Least Squares (PLS)

data("plethShapeFood")
Y.gpa <- gpagen(plethShapeFood$land,
                print.progress = FALSE,)
food <- plethShapeFood$food
rownames(food) <- names(Y.gpa$Csize) # just to assure match

PLSfood <- two.b.pls(food, Y.gpa$coords, 
                     print.progress = FALSE,
                     iter = 9999)
plot(PLSfood)

## GLM: Regression


fit.null <- procD.lm(coords ~ 1, data = Y.gpa,
                     iter = 9999)
fit.alt <- procD.lm(coords ~ log(Csize), data = Y.gpa,
                    turbo = FALSE, iter = 9999)
anova(fit.alt)
plot(fit.alt, type = "regression", reg.type = "RegScore", predictor = Y.gpa$Csize)

## Allometry

data(pupfish)
plotAllSpecimens(pupfish$coords)  #NOTE: already GPA-aligned
#Y.gpa <- gpagen(pupfish$coords, print.progress = FALSE)    #GPA-alignment  
pupfish$logSize <- log(pupfish$CS)
pupfish$Group <- interaction(pupfish$Pop, pupfish$Sex)

fit <- procD.lm(coords ~ logSize, data = pupfish, print.progress = FALSE) 
anova(fit)

plot(fit, type = "regression", reg.type = "RegScore", predictor = pupfish$logSize, pch = 19)
 #NOTE: Try also: reg.type = "PredLine"

#### Comparing Allometric Trajectories

fit.common <- procD.lm(coords ~ logSize + Group, 
                       data = pupfish, print.progress = FALSE) 
fit.unique <- procD.lm(coords ~ logSize * Group, 
                       data = pupfish, print.progress = FALSE)  
anova(fit.unique)

par(mfcol = c(1,2))
plot(fit.common, type = "regression", predictor = pupfish$logSize, 
     reg.type = "PredLine", pch=19, col = pupfish$Group)
legend("topleft", legend = unique(pupfish$Group), pch = 21, pt.bg = unique(pupfish$Group))
mtext("Common Slopes")
plot(fit.unique, type = "regression", predictor = pupfish$logSize, 
     reg.type = "PredLine", pch=19, col = pupfish$Group)
legend("topleft", legend = unique(pupfish$Group), pch = 21, pt.bg = unique(pupfish$Group))
mtext("Unique Slopes")
par(mfcol = c(1,1))
