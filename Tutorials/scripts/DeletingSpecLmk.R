#Code and analyses for 2.7-DeletingSpecLmks

## ----prelims----

library(geomorph)
data(plethodon)
data <- plethodon$land

## ----rmsingle----

data.mod <- data[,,-4]

## ----rmmulti----

data.mod <- data[,,-(4:8)]
dim(data.mod)

## ----rmmultioutseq----

omit <- c(4,9,15,28)
data.mod <- data[,,-omit]
dim(data.mod)

## ----rmlmk----

data.mod <- data[-4,,]
dim(data.mod)

## ----rmmultilmk----

data.mod <- data[-(4:8),,]
dim(data.mod)

## ----rmmultilmkoutseq----

omit <- c(2,5,7)
data.mod <- data[-omit,,]
dim(data.mod)