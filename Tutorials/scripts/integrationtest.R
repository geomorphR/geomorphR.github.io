#Code and analyses for 3.2.2-integration.test

## ----prelims----

library(geomorph)
data("plethodon")
lmks <- gpagen(plethodon$land)

## ----twopart----

lmkpart <- c("A","A","A","A","A","B","B","B","B","B","B","B")
IT <- integration.test(lmks$coords, partition.gp=lmkpart, iter=999, print.progress = F)
summary(IT) 

## ----twopartplot----

plot(IT)

## ----threepart----

lmkpart2 <- c("A","A","A","A","B","B","B","B","C","C","C","C")
IT2 <- integration.test(lmks$coords, partition.gp=lmkpart2, iter=999, print.progress = F)

## ----threepartsum----

summary(IT2)