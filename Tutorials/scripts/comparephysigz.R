#Code and analyses for 3.3.5-comparephysigz

## ----prelims----

library(geomorph)
data(plethspecies) 

Y.gpa<-gpagen(plethspecies$land)

jaw <- 1:5
cranium <- 6:11

PS.jaw <- physignal.z(A = Y.gpa$coords[jaw,,], phy = plethspecies$phy, 
                      lambda = "front", PAC.no = 7, iter=999)
PS.cranium <- physignal.z(A = Y.gpa$coords[cranium,,], phy = plethspecies$phy, 
                          lambda = "front", PAC.no = 7, iter=999)
PS.list <-list(PS.jaw, PS.cranium)
names(PS.list) <- c("jaw", "cranium")

## ----list----

PS.list <-list(PS.jaw, PS.cranium)
names(PS.list) <- c("jaw", "cranium")


## ----comparephysig----

PS.Z <- compare.physignal.z(PS.list)


## ----sum----

summary(PS.Z)