## ----readdata----

#load geomorph
library(geomorph)

#read in landmark data
plethdata <- readland.tps("Data-figures/plethland.tps", warnmsg = F)

#read in site and species variables
site <- factor(as.matrix(read.csv("Data-figures/plethsite.csv", header = T)))
species <- factor(as.matrix(read.csv("Data-figures/plethspec.csv", header = T)))


## ----GPA----

gpa <- gpagen(plethdata, print.progress = F)

## ----GDF----

#create geomorph data frame
gdf <- geomorph.data.frame(gpa, site = site, species = species)

#create a factor that includes all combinations of site and species
gdf$Group <- interaction(gdf$site, gdf$species)

## ----ANOVA----

fit <- procD.lm(coords ~ species * site, 
                 data = gdf, iter = 999, turbo = TRUE,
                 RRPP = TRUE, print.progress = FALSE)

## ----summary1----

summary(fit)


## ----plotfit----

fitplot <- plot(fit, pch = 21, bg = gdf$Group, type = "PC")
legend("topright", legend = levels(gdf$Group), pch = 21, pt.bg = 1:4)

## ----pairwise----

pairs <- pairwise(fit, groups = gdf$Group)

## ----summary2----

summary(pairs)

## ----shapepred----

X <- fit$X
X <- X[,-1]
symJord <- c(0,1,0) # design for P. Jordani in sympatry
alloJord <- c(0,0,0) # design for P. Jordani in allopatry
pred <- shape.predictor(arrayspecs(fit$fitted, 12,2), x = X, symJord=symJord, alloJord=alloJord, 
                        Intercept = T)

## ----predcoords----

pred$symJord

## ----plotref----

M <- gpa$consensus

par(mfrow=c(1,2))
plotRefToTarget(M, pred$symJord, mag=2)
plotRefToTarget(M, pred$alloJord, mag=2)

## ----links----

plethodon$links

## ----method----

par(mfrow=c(1,3))
plotRefToTarget(M, pred$symJord, mag=2, method = "TPS" )
plotRefToTarget(M, pred$symJord, mag=2, method = "vector")
plotRefToTarget(M, pred$symJord, mag=2, method = "points")

