#Code and analyses for 3.2.10-compareZVrel

## ----prelims----

library(geomorph)
data("plethodon")
lmks <- gpagen(plethodon$land)

## ----subset----

coords.gp <- coords.subset(lmks$coords, plethodon$species)

## ----integ----

Vrel.gp <- Map(function(x) integration.Vrel(x), coords.gp)

## ----comp----

out <- compare.ZVrel(Vrel.gp$Jord, Vrel.gp$Teyah)
summary(out)