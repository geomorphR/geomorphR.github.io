#Code and analyses for 2.4-CombiningArrays

## ----prelims----

library(geomorph)
library(abind)
data("plethodon")
Array1 <- plethodon$land[,,1]
Array2 <- plethodon$land[,,2]

## ----abindalong3----

Array_Com <- abind(Array1, Array2, along=3)
Array_Com

## ----abindalong1----

Array_Com <- abind(Array1, Array2, along=1)
Array_Com


