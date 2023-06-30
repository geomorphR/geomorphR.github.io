#Code and analyses for 1.2- EstMissing

## ----prelims----

library(knitr)
library(geomorph)
library(Morpho)
#opts_chunk$set(echo = TRUE)

## ----sampledata----

#read data
mydat<-readland.tps("./Data-figures/LizardShape.tps",warnmsg = FALSE)
inc.shape <- gpagen(mydat,print.progress = FALSE)$coords
mydat <- gpagen(mydat, print.progress = FALSE)$coords

#landmark pairing
land.prs<-matrix(c(13, 14,2,11,15, 16,27, 28,3,10,17,19,4, 9,20, 21,22, 24,25, 26,5, 8, 6 ,7),ncol=2,byrow=T)

#plotPCA
links<-read.table("./Data-figures/lizardlinks.txt",header=T)
ref<-mshape(mydat)
shape.pca <- gm.prcomp(mydat)
col.spec <- rep("black",dim(mydat)[[3]]); col.spec[62] ="red"
sz.spec <- rep(1.0,dim(mydat)[[3]]); sz.spec[62] =2

## ----plotallspecimens----

par(mfrow=c(1,2)) 
plotAllSpecimens(mydat,links=links)
plot(shape.pca,pch=21, bg=col.spec, cex=sz.spec)
par(mfrow=c(1,1)) 

## ----lmkpairs----

land.prs

## ----deletelmksliz----

inc.shape[c(5,28,14,4),,62]<-NA

## ----missinglmksliz----

include_graphics("Data-figures/missing/LizardMissing.png")

##----specframe----

inc.shape[,,62]

## ----estmissingliz----

inc.shape <- fixLMmirror(inc.shape, land.prs)

## ----plotcompareliz1----

par(mfrow=c(1,2)) 
plotRefToTarget(mydat[,,62],mydat[,,62],links=links)
mtext("Original Specimen")
plotRefToTarget(mydat[,,62],inc.shape[,,62],links=links)
mtext("TPS: Original --> Estimated Specimen")

## ----plotcompareliz2----
par(mfrow=c(1,1)) 
dist(rbind(t(matrix(inc.shape[,,62])),t(matrix(mydat[,,62]))))

## ----plethdata----

data("plethodon")
data("plethspecies")
GPA <- gpagen(plethodon$land)

## ----PCAandplot----

PCA <- gm.prcomp(GPA$coords)
ref <- GPA$consensus
PC <- PCA$x[,1]
preds <- shape.predictor(GPA$coords, x= PC, Intercept = FALSE, 
                         pred1 = min(PC), pred2 = max(PC))

par(mfrow=c(2,2)) 
plot(PCA, pch=21, cex=1,bg="black")
plotRefToTarget(ref,ref, links = plethodon$links)
plotRefToTarget(ref, preds$pred1, links = plethodon$links)
mtext("PC1-Negative")
plotRefToTarget(ref, preds$pred2, links = plethodon$links)
mtext("PC1-Positive")
par(mfrow=c(1,1))


## ----deletelmkspleth----

pleth.col <- rep("black",dim(GPA$coords)[[3]]); pleth.col[c(1,20,30)] ="red"
pleth.sz <- rep(1.0,dim(GPA$coords)[[3]]); pleth.sz[c(1,20,30)] =2

plot(PCA,pch=21, bg=pleth.col, cex=pleth.sz)

shapes.missing<-GPA$coords  #delete 1, 7, 15, 26 
shapes.missing[c(1,5,8),,1]<-NA
shapes.missing[c(1,5,8),,20]<-NA
shapes.missing[c(1,5,8),,30]<-NA

## ----estmissingpleth----

new.tps<-estimate.missing(shapes.missing,method="TPS")

## ----plotcomparepleth----

par(mfrow=c(2,2))
plotRefToTarget(ref,ref,links=plethodon$links)
mtext("Reference")
plotRefToTarget(ref,GPA$coords[,,1],links=plethodon$links)
mtext("TPS: Reference --> Original")
plotRefToTarget(ref,new.tps[,,1],links=plethodon$links)
mtext("TPS: Ref --> Estimated Specimen")
plotRefToTarget(GPA$coords[,,1],new.tps[,,1],links=plethodon$links)
mtext("TPS: Original --> Estimated Specimen")

par(mfrow=c(1,1)) 

as.matrix(dist(rbind(t(matrix(ref)),t(matrix(GPA$coords[,,1])),t(matrix(new.tps[,,1])))))

## ----estmissingreg----

new.reg<-estimate.missing(shapes.missing,method="Reg")
new.reg<-gpagen(new.reg, print.progress=FALSE)$coords

## ----plotcomparereg1----

par(mfrow=c(2,2))
plotRefToTarget(ref,ref,links=plethodon$links)
mtext("Reference")
plotRefToTarget(ref,GPA$coords[,,1],links=plethodon$links)
mtext("TPS: Reference --> Original")
plotRefToTarget(ref,new.reg[,,1],links=plethodon$links)
mtext("TPS: Ref --> Estimated Specimen")
plotRefToTarget(GPA$coords[,,1],new.reg[,,1],links=plethodon$links)
mtext("TPS: Original --> Estimated Specimen")

par(mfrow=c(1,1)) 

## ----plotcomparereg2----

as.matrix(dist(rbind(t(matrix(ref)),t(matrix(GPA$coords[,,1])),t(matrix(new.tps[,,1])))))
