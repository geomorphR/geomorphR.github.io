#Code and analyses for 2.8-MatchSpectoPhy

## ----prelims----

library(geomorph)
library(geiger)
data("plethspecies")
pleth <- plethspecies
dimnames(pleth$land)[[3]] <- pleth$phy$tip.label

## ----datbyrow----

ex <- matrix(rep(1, 50), ncol = 10) 
colnames(ex)<- c("X1","Y1","X2","Y2","X3","Y3","X4","Y4","X5","Y5")
rownames(ex)<- c("Species A", "Species A", "Species B", "Species C", "Species D")
ex

## ----twodarray----

treematch <- treedata(pleth$phy, two.d.array(pleth$land), sort = TRUE)
pleth$phy <- arrayspecs(treematch$data, 11, 2)



