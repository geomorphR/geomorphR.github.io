#Code and analyses for CombineLmkConfig

## ----prelims----

library(geomorph)
data("plethodon")
lmk1 <- gpagen(plethodon$land[1:6,,])
lmk2 <- gpagen(plethodon$land[7:12,,])

## ----comb----

comb <- combine.subsets(lmk1, lmk2)
comb