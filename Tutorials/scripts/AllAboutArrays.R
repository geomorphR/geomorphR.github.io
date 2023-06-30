#Code and analyses for 2.1-AllAboutArrays

## ----prelims----

library(geomorph)

data("lizards")

## ----array----

Array <- lizards$coords[1:10,,1:5]
Array[,,1:2]

## ----lmks----

Array[2,,]

## ----xyz----

Array[,1,]

## ----specimen----

Array[,,1]

## ----multi1----

Array[,,1:3]

## ----multi2----

Array[,,c(1,3,5)]