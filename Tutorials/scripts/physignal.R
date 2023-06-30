#Code and analyses for 3.3.3-physignal

## ----prelims----

library(geomorph)
data("plethspecies")
lmks <- gpagen(plethspecies$land)

## ----physigshape----

PS.shape <- physignal(A = lmks$coords, phy = plethspecies$phy, iter=999)

## ----sumshape----

summary(PS.shape)

## ----plotshape----

plot(PS.shape)
plot(PS.shape$PACA, phylo = TRUE)

## ----physigprofile----

PS.shape$K.by.p

## ----physigsize----

PS.size <- physignal(A = lmks$Csize, phy = plethspecies$phy, iter=999)

## ----sumsize----

summary(PS.size)

## ----plotsize----

plot(PS.size)