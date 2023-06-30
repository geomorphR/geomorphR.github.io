#Code and analyses for 2.6-SwappingLandmarks

## ----prelims----

library(geomorph)
data("mosquito")
data <- gpagen(mosquito$wingshape)$coords
data[c(1,7),,20] <- data[c(7,1),,20]

## ----plotoutliers----

plotOutliers(gpagen(data)$coords)

## ----plotRef----

plotRefToTarget(data[,,20], mshape(data), method = "vector", label = T)

## ----swaptwo----

data[c(1,7),,20] <- data[c(7,1),,20]
plotRefToTarget(data[,,20], mshape(data), method = "vector")

## ----swapmulti----

data <- gpagen(mosquito$wingshape)$coords
data[c(1,3,7,8),,c(4,8)] <- data[c(3,1,8,7),,c(4,8)]
data[c(1,9),,9] <- data[c(9,1),,9]

## ----lmkerrors----

par(mar = c(5.1,2.1,2.1,2.1))
par(mfrow=c(3,1))
plotRefToTarget(data[,,4], mshape(data), method = "vector", label = T)
plotRefToTarget(data[,,8], mshape(data), method = "vector", label = T)
plotRefToTarget(data[,,9], mshape(data), method = "vector", label = T)

## ----swap----

data[c(1,3,7,8),,c(4,8)] <- data[c(3,1,8,7),,c(4,8)]
data[c(1,9),,9] <- data[c(9,1),,9]

## ----plot----

par(mar = c(5.1,2.1,2.1,2.1))
par(mfrow=c(3,1))
plotRefToTarget(data[,,4], mshape(data), method = "vector", label = T)
plotRefToTarget(data[,,8], mshape(data), method = "vector", label = T)
plotRefToTarget(data[,,9], mshape(data), method = "vector", label = T)
