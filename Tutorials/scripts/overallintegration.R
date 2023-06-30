#Code and analyses for 3.2.4-overallintegration

## ----prelims----

library(geomorph)
data("plethspecies")
lmks <- gpagen(plethspecies$land)
phy <- plethspecies$phy

## ----int----

int <- integration.Vrel(lmks$coords)
int

## ----intphy----

int <- integration.Vrel(lmks$coords, phy = phy)
int