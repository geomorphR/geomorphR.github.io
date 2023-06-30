#Code and analyses for 3.2.6-phylomodularity

## ----prelims----

library(geomorph)
data("plethspecies")
lmks <- gpagen(plethspecies$land)
phylo <- plethspecies$phy

## ----part----

lmkpart <- c("A","A","A","A","A","B","B","B","B","B","B")
MT <- phylo.modularity(lmks$coords, phy = phylo, partition.gp=lmkpart, CI = F, iter=999, print.progress = F)

## ----sum----

summary(MT) 

## ----plot----

plot(MT)