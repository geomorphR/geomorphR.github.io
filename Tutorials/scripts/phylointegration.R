#Code and analyses for 3.2.5-phylointegration

## ----prelims----

library(geomorph)
data("plethspecies")
lmks <- gpagen(plethspecies$land)
phylo <- plethspecies$phy

## ----part----

lmkpart <- c("A","A","A","A","A","B","B","B","B","B","B")
IT <- phylo.integration(lmks$coords, phy = phylo, partition.gp=lmkpart, iter=999, print.progress = F)

## ----sum----

summary(IT) 

## ----plot----

plot(IT)