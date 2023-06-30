#Code and analyses for Interlmkdist

## ----prelims----

library(geomorph)
data("plethodon")
lmk <- plethodon$land[1:5,,1:5]

## ----defdists----

dists <- matrix(c(1,2,3,4),ncol = 2, byrow = T)
colnames(dists) <- c("start", "end")
dists

## ----calcdists----

intlmkdist <- interlmkdist(lmk, dists)
intlmkdist

## ----lmkvector----

lmkno <- c(1:dim(lmk)[1]) #vector of 1:number of landmarks
lmkno

## ----defalldists----

alldist <- matrix(combn(lmkno,2, c),ncol = 2, byrow = T) #returns all pairwise combinations of landmark numbers
colnames(alldist) <- c("start", "end")
alldist

## ----calcalldists----

intlmkdist <- interlmkdist(lmk, alldist)
intlmkdist