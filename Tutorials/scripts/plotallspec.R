#Code and analyses for 4.1-plotallspec

## ----prelims----

library(geomorph)
data("plethodon")
data("scallops")

## ----2dplot----

plotAllSpecimens(gpagen(plethodon$land, print.progress = F)$coords)

## ----links----

plethodon$links

## ----2dplotlink----

plotAllSpecimens(gpagen(plethodon$land, print.progress = F)$coords, links = plethodon$links)

## ----3dplot----

plotAllSpecimens(gpagen(scallops$coorddata, print.progress = F)$coords)
rglwidget()