#Code and analyses for shapepred

## ----prelims----

library(geomorph)
data("plethodon")
lmks <- gpagen(plethodon$land )
PCA <- gm.prcomp(lmks$coords)
data(plethShapeFood) 
plethfood<-gpagen(plethShapeFood$land)

spec <- plethodon$species
site <- plethodon$site

df <- geomorph.data.frame(lmks$coords, spec = spec, site = site)

fit <- procD.lm(lmks$coords ~ spec * site, 
                data = df, iter = 999, turbo = TRUE,
                RRPP = TRUE, print.progress = FALSE)

## ----PCs----

PC <- PCA$x[,1]
PC

## ----PCpred----

preds <- shape.predictor(lmks$coords, x= PC, Intercept = FALSE, 
                         pred1 = min(PC), pred2 = max(PC))

## ----PCvis1----

plotRefToTarget(lmks$consensus, preds$pred1)

## ----PCvis2----

plotRefToTarget(lmks$consensus, preds$pred2)

## ----allopred----

preds <- shape.predictor(lmks$coords, x= log(lmks$Csize), 
                         Intercept = TRUE, 
                         predmin = min(log(lmks$Csize)), 
                         predmax = max(log(lmks$Csize))) 

## ----allovis----

plotRefToTarget(lmks$consensus, preds$predmin, mag=3)
plotRefToTarget(lmks$consensus, preds$predmax, mag=3)

## ----genfit----

gdf <- geomorph.data.frame(lmks)
plethAllometry <- procD.lm(coords ~ log(Csize), data=gdf)

## ----regscore----

allom.plot <- plot(plethAllometry, type = "regression", predictor = log(gdf$Csize),
                   reg.type ="RegScore")
preds <- shape.predictor(plethAllometry$GM$fitted, 
                         x= allom.plot$RegScore, Intercept = FALSE, 
                         predmin = min(allom.plot$RegScore), 
                         predmax = max(allom.plot$RegScore))

plotRefToTarget(lmks$consensus, preds$predmin, mag=3)
plotRefToTarget(lmks$consensus, preds$predmax, mag=3)

## ----predline----

allom.plot <- plot(plethAllometry, type = "regression",predictor = log(gdf$Csize),
                   reg.type ="PredLine")

preds <- shape.predictor(plethAllometry$GM$fitted,
                         x= allom.plot$PredLine, Intercept = FALSE,
                         predmin = min(allom.plot$PredLine),
                         predmax = max(allom.plot$PredLine))

plotRefToTarget(lmks$consensus, preds$predmin, mag=3)
plotRefToTarget(lmks$consensus, preds$predmax, mag=3)

## ----PLS----

PLS <-two.b.pls(A1 = plethShapeFood$food, A2 = plethfood$coords, iter=999, print.progress = F) 

## ----PLSplot----

plot(PLS)

## ----PLSpred----

preds <- shape.predictor(plethfood$coords, plethShapeFood$food, 
                         Intercept = FALSE,
                         method = "PLS",
                         pred1 = 2, pred2 = -4, pred3 = 2.5)

## ----PLSvis---

plotRefToTarget(plethfood$consensus, preds$pred1, mag=2)
plotRefToTarget(plethfood$consensus, preds$pred2, mag=2)
plotRefToTarget(plethfood$consensus, preds$pred3, mag=2)

## ----ANOVA---

X <- fit$X
X <- X[,-1]
symJord <- c(0,1,0) # design for P. Jordani in sympatry
alloJord <- c(0,0,0) # design for P. Jordani in allopatry
pred <- shape.predictor(arrayspecs(fit$fitted, 12,2), x = X, symJord=symJord, alloJord=alloJord, 
                        Intercept = T)
## ----ANOVAvis---

plotRefToTarget(lmks$consensus, pred$symJord, mag=2)
plotRefToTarget(lmks$consensus, pred$alloJord, mag=2)
