#Code and analyses for 2.5-ReflectingLandmarks

## ----prelims----

library(geomorph)
data("mosquito")

## ----plotdata----

wings <- gpagen(mosquito$wingshape, print.progress = F)

plotAllSpecimens(wings$coords)

## ----reflectspecimens----

plotAllSpecimens(wings$coords*-1)

## ----onereflect----

wings$coords[,,4] <- wings$coords[,,4]*-1
plotAllSpecimens(wings$coords)

## ----onereflect2----

wings$coords[,,4] <- wings$coords[,,4]*-1
plotAllSpecimens(wings$coords)



