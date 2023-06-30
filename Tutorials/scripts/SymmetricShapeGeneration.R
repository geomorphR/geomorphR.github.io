#Code and analyses for 1.5-SymmetricShapeGeneration

## ----prelims----

library(geomorph)
data("lizards")
data("scallops")


## ----lizardbilat----

lizard.sym <- bilat.symmetry(lizards$coords, ind = lizards$ind, 
                             replicate = lizards$rep, object.sym = TRUE, 
                             land.pairs = lizards$lm.pairs, print.progress = FALSE)

## ----labels----

lizards$ind

## ----replicates----

lizards$rep

## ----pairs----

lizards$lm.pairs

## ----plotbilat----

plot(lizard.sym)

## ----symmshape----

lizard.sym$symm.shape