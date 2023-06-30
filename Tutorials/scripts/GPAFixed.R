## ----prelims----

options(rgl.useNULL = TRUE) 
library(rgl)
library(geomorph)
data("plethodon")
headshape <- readland.tps("./Data-figures/headshape.tps")

## ----GPA1----

GPA <- gpagen(plethodon$land, print.progress = FALSE)

plotAllSpecimens(GPA$coords)