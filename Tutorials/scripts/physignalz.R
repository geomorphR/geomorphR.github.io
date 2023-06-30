#Code and analyses for 3.3.4-physignalz

## ----prelims----

library(geomorph)
data("plethspecies")
lmks <- gpagen(plethspecies$land)

## ----physigzshape----

PS.shape <- physignal(A = lmks$coords, phy = plethspecies$phy, iter=999)

## ----physigzshapesum----

summary(PS.shape)

## ----physigzshapeplot----

plot(PS.shape)
plot(PS.shape$PACA, phylo = TRUE)

## ----physigzprofile----

PS.shape$K.by.p

## ----physigzsize----

PS.size <- physignal(A = lmks$Csize, phy = plethspecies$phy, iter=999)

## ----physigzsizesum----

summary(PS.size)

## ----physigzsizeplot----

plot(PS.size)