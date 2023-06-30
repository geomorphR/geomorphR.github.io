## ----prelims----

options(rgl.useNULL = TRUE) 
library(rgl)
library(geomorph)
data("plethodon")
headshape <- readland.tps("./Data-figures/headshape.tps")

## ----plotallspec----

plotAllSpecimens(plethodon$land)

