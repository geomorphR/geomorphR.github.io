#Code and analyses for 3.2.9-comparepls

## ----prelims----

library(geomorph)
data("pupfish")
group <- factor(paste(pupfish$Pop, pupfish$Sex, sep = "."))
tail.LM <- c(1:3, 5:9, 18:38)
head.LM <- (1:56)[-tail.LM]
tail.coords <- pupfish$coords[tail.LM,,]
head.coords <- pupfish$coords[head.LM,,]
tail.coords.gp <- coords.subset(tail.coords, group)
head.coords.gp <- coords.subset(head.coords, group)

## ----group----

levels(group)

## ----integ----

integ.tests <- Map(function(x,y) integration.test(x, y, iter=499, 
                                                  print.progress = FALSE), head.coords.gp, tail.coords.gp)

## ----compare----

group.Z <- compare.pls(integ.tests)
summary(group.Z)

