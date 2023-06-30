#Code and analyses for ordplot

## ----prelims----

library(geomorph)
data("plethspecies")
gpa <- gpagen(plethspecies$land)
phy <- plethspecies$phy

## ----PCA----

PCA <- gm.prcomp(gpa$coords)
summary(PCA)

## ----PCAplot----

plot(PCA)

## ----PCAanst----

PCA <- gm.prcomp(gpa$coords, phy = phy, transform = F)
plot(PCA, phylo = T)

## ----PCA3d----

plot(PCA, time.plot = TRUE, bg = "red", 
     phylo.par = list(tip.labels = TRUE, 
                      tip.txt.cex = 2, edge.color = "blue", edge.width = 2))

## ----pPCA----

PCA <- gm.prcomp(gpa$coords, phy = phy, GLS = T)
plot(PCA, phylo = T)

## ----pPCAtran----

PCA <- gm.prcomp(gpa$coords, phy = phy, GLS = T, transform = T)
plot(PCA, phylo = T)

## ----PaCA----

PCA <- gm.prcomp(gpa$coords, phy = phy, align.to.phy = T)
plot(PCA, phylo = T)

## ----PaCAgls----

PCA <- gm.prcomp(gpa$coords, phy = phy, align.to.phy = T, GLS = T)
plot(PCA, phylo = T)