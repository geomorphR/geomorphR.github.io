## ----prelims----

options(rgl.useNULL = TRUE) 
library(rgl)
library(geomorph)
data("plethodon")
headshape <- readland.tps("./Data-figures/headshape.tps")

## ----curvedata----

data(hummingbirds)
hummingbirds$curvepts

## ----GPA2----

GPA <- gpagen(hummingbirds$land, curves = hummingbirds$curvepts, 
              approxBE = TRUE, print.progress = FALSE)
plotAllSpecimens(GPA$coords)

## ----sliders----

Sliders <- define.sliders(1:10)
Sliders

## ----slidersmulti----

Sliders <- rbind(define.sliders(1:5), define.sliders(6:10))
Sliders