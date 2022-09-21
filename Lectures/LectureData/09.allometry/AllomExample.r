# Recreate figures for Lecture 4: shape spaces, TPS, PCA

library(geomorph)
lizards <- readland.tps('symm.shape2.tps', warnmsg = FALSE)
lizards2 <- lizards +1
writeland.tps(lizards2,"symm.shape2.tps")

liz.lab <- read.csv('ind.info.txt',header=TRUE, sep=",")
Y.gpa <- gpagen(lizards, print.progress = FALSE)
CS <- liz.lab$cs
type <- liz.lab$type
gdf <- geomorph.data.frame(Y.gpa, CS = CS, type = type)

plotAllSpecimens(Y.gpa$coords)

fit <- procD.lm(Y.gpa$coords~CS*type, data = gdf, print.progress = FALSE)

anova(fit)

plotTangentSpace(Y.gpa$coords, groups = type, warpgrids = FALSE)

#plots: regscore and predline

plot(fit, type = "regression", predictor = gdf$CS, reg.type = "RegScore",
     pch=21, bg=gdf$type)

plot(fit, type = "regression", predictor = gdf$CS, reg.type = "PredLine", 
     pch=21, bg=gdf$type)
