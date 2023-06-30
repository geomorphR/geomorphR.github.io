#Code and analyses for 3.2.3-modularitytest

## ----prelims----

library(geomorph)
data("pupfish")
lmks <- gpagen(pupfish$coords)

## ----partition----

land.gps<-rep('a',56); land.gps[39:48]<-'b'

## ----modularity----

MT <- modularity.test(lmks$coords,land.gps,CI=FALSE,iter=99, print.progress = F)

## ----sumplot----

summary(MT)
plot(MT)