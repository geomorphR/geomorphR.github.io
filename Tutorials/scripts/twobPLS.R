#Code and analyses for 3.2.1-twobpls

## ----prelims----

library(geomorph)
data("plethShapeFood")
gpa <- gpagen(plethShapeFood$land)
food <- plethShapeFood$food

## ----PLS----

PLS <- two.b.pls(gpa$coords, food, iter = 999, print.progress = F)

## ----PLSsum----

summary(PLS)

## ----PLSplot----

plot(PLS)

## ----compPLS----

PLS1 <- two.b.pls(gpa$coords[,,1:30], food[1:30,], iter = 999, print.progress = F)
PLS2 <- two.b.pls(gpa$coords[,,31:69], food[31:69,], iter = 999, print.progress = F)

## ----compPLSsum----

compare <- compare.pls(PLS1,PLS2)
summary(compare)

