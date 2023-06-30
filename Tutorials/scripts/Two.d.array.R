#Code and analyses for Two.d.array

## ----prelims----

library(geomorph)
data("plethodon")
lmks <- plethodon$land[1:5,,1:3]
dimnames(lmks)[[3]] <- c("Specimen1", "Specimen2", "Specimen3")

## ----array----

lmks

## ----matrix----

lmks <- two.d.array(lmks)
lmks