#Code and analyses for 3.1-GeneralizedProcrustesAnalysis

## ----prelims----

options(rgl.useNULL = TRUE) 
library(rgl)
library(geomorph)
data("plethodon")
headshape <- readland.tps("./Data-figures/headshape.tps")

## ----plotallspec----

plotAllSpecimens(plethodon$land)

## ----GPA1----

GPA <- gpagen(plethodon$land, print.progress = FALSE)

plotAllSpecimens(GPA$coords)

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

## ----surfdata----

data("scallops")
scallops$surfslide

## ----GPA3----

GPA <- gpagen(scallops$coorddata,curves = scallops$curvslide, surfaces = scallops$surfslide, print.progress = FALSE)

plotAllSpecimens(GPA$coords)
rglwidget()

## ----subset----

GPA$consensus

