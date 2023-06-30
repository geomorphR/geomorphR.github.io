#Code and analyses for 3.3.1-compareevolrates

## ----prelims----

library(geomorph)
data("plethspecies")
lmks <- gpagen(plethspecies$land)

## ----factor----

gp.end<-factor(c(0,0,1,0,0,1,1,0,0))
names(gp.end)<-plethspecies$phy$tip

## ----evolrate----

ER<-compare.evol.rates(A = lmks$coords, phy = plethspecies$phy,
                       method = "simulation",gp = gp.end,iter = 999, print.progress = F)

## ----sum----

summary(ER)

## ----plot----

plot(ER)