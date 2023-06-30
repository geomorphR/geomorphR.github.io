#Code and analyses for 3.2.7-globalintegration

## ----prelims----

library(geomorph)
data("plethodon")
lmks <- gpagen(plethodon$land)

## ----globalint----

int <- globalIntegration(lmks$coords)

int

