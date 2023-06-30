#Code and analyses for plotspec

## ----prelims----

library(geomorph)
data(scallopPLY)
mesh <- scallopPLY$ply
lmks <- scallopPLY$coords

## ----default----

plotspec(spec = mesh, digitspec = lmks, fixed = 16, centered = T)
rglwidget()

## ----color----

plotspec(spec = mesh, digitspec = lmks, fixed = 16, centered = T, fixed.pt.col = "blue")
rglwidget()