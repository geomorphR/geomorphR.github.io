#Code and analyses for 3.3.2-comparemultievolrates

## ----prelims----

library(geomorph)
data("plethspecies")
lmks <- gpagen(plethspecies$land)

## ----groups----

land.gp<-c("A","A","A","A","A","B","B","B","B","B","B") 

## ----multievol----

EMR<-compare.multi.evol.rates(A = lmks$coords,gp = land.gp, 
                              Subset = TRUE, phy = plethspecies$phy,iter=999, print.progress = F)

## ----sum----

summary(EMR)

## ----plot----

plot(EMR)