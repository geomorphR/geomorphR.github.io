#Code and analyses for 1.3-IdentifyOutliers

## ----prelims----

library(geomorph)
data(plethodon)

## ----readdata----

newland <- plethodon$land
#swap lmks
newland[c(1,8),,2] <- newland[c(8,1),,2]
newland[c(3,11),,26] <- newland[c(11,3),,26]

## ----plotoutliers----

Y <- gpagen(newland, print.progress = FALSE)
plotOutliers(Y$coords, inspect.outliers = T)

## ----swaplmks----

newland[c(1,8),,2] <- newland[c(8,1),,2]
newland[c(3,11),,26] <- newland[c(11,3),,26]
Y <- gpagen(newland, print.progress = FALSE)

## ----replotoutliers----

plotOutliers(Y$coords, inspect.outliers = T)