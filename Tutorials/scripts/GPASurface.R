
## ----prelims----

options(rgl.useNULL = TRUE) 
library(rgl)
library(geomorph)
data("plethodon")
headshape <- readland.tps("./Data-figures/headshape.tps")

## ----surfdata----

data("scallops")
scallops$surfslide

## ----GPA3----

GPA <- gpagen(scallops$coorddata,curves = scallops$curvslide, surfaces = scallops$surfslide, print.progress = FALSE)

plotAllSpecimens(GPA$coords)

bg3d(texture = "Data-figures/3Dbg.png", col="white")


## ----subset----

GPA$consensus