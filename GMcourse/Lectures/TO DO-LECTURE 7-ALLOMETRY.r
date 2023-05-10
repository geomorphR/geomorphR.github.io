lizards <- readland.tps('LectureData/07.allometry/symm.shape2.tps', warnmsg = FALSE)
liz.lab <- read.csv('LectureData/07.allometry/ind.info.txt',header=TRUE, sep=",")
col.gp <- rep("black",nrow(liz.lab))
col.gp[which(liz.lab$type=='U')] <- 'red'
Y.gpa <- gpagen(lizards, print.progress = FALSE)
CS <- liz.lab$cs
type <- as.factor(liz.lab$type)
gdf <- geomorph.data.frame(Y.gpa, CS = CS, type = type)

plotAllSpecimens(Y.gpa$coords)

fit <- procD.lm(Y.gpa$coords~CS*type, data = gdf, print.progress = FALSE)
anova(fit)$table

PCA <- gm.prcomp(Y.gpa$coords)
plot(PCA,pch = 21, bg=col.gp,cex = 2)
legend("topleft", levels(gdf$type), pch = 21, pt.bg = unique(col.gp))

par(mfrow=c(1,2)) 
plot(fit, type = "regression", predictor = gdf$CS, 
     reg.type = "RegScore", pch=19, col = col.gp)
legend("topleft", levels(gdf$type), pch = 21, pt.bg = unique(col.gp))
mtext("Regression Scores")
plot(fit, type = "regression", predictor = gdf$CS, 
     reg.type = "PredLine", pch=19, col = col.gp)
legend("topleft", levels(gdf$type), pch = 21, pt.bg = unique(col.gp))
mtext("Predicted Lines")
par(mfrow=c(1,1)) 

# SET UP SHAPE PREDICTOR FOR ANCOVA example (LIZARD for lecture and lab)

#use fit$X in some way

preds <- shape.predictor(fit$GM$fitted, x= gdf$CS, Intercept = TRUE, 
                         predmin.u = min(gdf$CS), 
                         predmax.u = max(gdf$CS),
                         predmin.r = min(gdf$CS), 
                         predmax.r = max(gdf$CS)) 
