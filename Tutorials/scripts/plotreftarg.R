#Code and analyses for plotreftarg

## ----prelims----

library(geomorph)
data(plethodon)
links <- plethodon$links
GPApleth<- gpagen(plethodon$land)
data("scallopPLY")
mesh <- scallopPLY$ply
data("scallops")
GPAscal <- gpagen(scallops$coorddata)

## ----tps----

plotRefToTarget(GPApleth$consensus, GPApleth$coords[,,1], links = links, method = "TPS")

## ----vec----

plotRefToTarget(GPApleth$consensus, GPApleth$coords[,,1], links = links, method = "vector")

## ----point----

plotRefToTarget(GPApleth$consensus, GPApleth$coords[,,1], links = links, method = "points")

## ----surf----

plotRefToTarget(GPAscal$consensus, GPAscal$coords[,,1], mesh = mesh, method = "surface")
rglwidget()
