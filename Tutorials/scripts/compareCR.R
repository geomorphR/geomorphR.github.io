#Code and analyses for 3.2.9-compareCR

## ----prelims----

library(geomorph)
data("pupfish")
lmks<-gpagen(pupfish$coords, print.progress = FALSE)
land.gps<-rep('a',56); land.gps[39:48]<-'b'
group <- factor(paste(pupfish$Pop, pupfish$Sex, sep = "."))
coords.gp <- coords.subset(lmks$coords, group)
land.gps3 <- rep('a',56); land.gps3[39:48]<-'b'; 
land.gps3[c(6:9,28:38)] <- 'c'
land.gps4 <- rep('a',56); land.gps4[39:48]<-'b'; 
land.gps4[c(6:9,28:38)] <- 'c'; 
land.gps4[c(10,49:56)] <- 'd'

## ----group----

levels(group)

## ----module----

modul.tests <- Map(function(x) modularity.test(x, land.gps,iter=999, print.progress = FALSE), coords.gp)

## ----compare----

group.Z <- compare.CR(modul.tests, CR.null = FALSE)
summary(group.Z)

## ----altmod----

m3.test <- modularity.test(coords.gp$Marsh.F,land.gps3, iter = 499, 
                           print.progress = FALSE)

m4.test <- modularity.test(coords.gp$Marsh.F,land.gps4, iter = 499, 
                           print.progress = FALSE)

## ----altcomp----

model.Z <- compare.CR(modul.tests$Marsh.F,m3.test,m4.test, 
                      CR.null = TRUE)

summary(model.Z)