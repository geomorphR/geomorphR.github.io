##Missing Data examples
library(geomorph)

#Symmetry: Lizard heads (Lazic et al. 2015)
mydat<-readland.tps("LizardShape.tps",warnmsg = FALSE)
shape <- gpagen(mydat,print.progress = FALSE)$coords
land.prs<-matrix(c(13, 14,2,11,15, 16,27, 28,3,10,17,19,4, 9,20, 21,22, 24,25, 26,5, 8, 6 ,7),ncol=2,byrow=T)

links<-read.table("lizardlinks.txt",header=T)
plotAllSpecimens(shape,links=links)
ref<-mshape(shape)
shape.pca <- gm.prcomp(shape)
col.spec <- rep("black",dim(shape)[[3]]); col.spec[62] ="red"
sz.spec <- rep(1.0,dim(shape)[[3]]); sz.spec[62] =2
plot(shape.pca,pch=21, bg=col.spec, cex=sz.spec)

plotRefToTarget(ref,ref,label = TRUE,links=links)  #delete: 5, 28, 14, 4
      #REFLECTED ONES ARE: 8, 27, 13, 9

plotRefToTarget(ref,ref,links=links)
plotRefToTarget(shape[,,62],shape[,,62],links=links)

flipped<-spec<-shape[,,62]
flipped[,2]<-flipped[,2]*-1  #reflect about Y-axis
  spec[c(5,28,14,4),]<-NA  #create missing
  spec[c(5,28,14,4),]<-flipped[c(8,27,13,9),]  #use mirrored landmarks

plotRefToTarget(shape[,,62],spec,links=links)
dist(rbind(t(matrix(spec)),t(matrix(shape[,,62]))))

rm(list = ls())
##########################
### Remaining methods: Simulate data
spec<-readland.tps('fish.tps', specID = "imageID", warnmsg = FALSE)
spec<-geomorph:::center.scale(spec[,,1])$coords
links<-read.table("links.txt")

#Simulate specimens.  Affine shape transformation: square-trapezoid
  # NOTE: DCA STILL A BIT UNHAPPY.  scaling of isotropic error not ideal, but ok. 
CS<-seq(.5,1.5, by =.01)  #size
Dil<-seq(.5,1.5, by =.01)  #size
n<-length(CS); p<-40; k<-2
specs<-array(NA,dim=c(p,k,n))
  for (i in 1:n){
    trans<-matrix(c(Dil[i],0,0,1),nrow=2)
    specs[,,i]<-CS[i]*(spec%*%trans)+.3*CS[i]*matrix(rnorm(n=p*k, sd = 0.01),ncol=2)  
  }
#plotAllSpecimens(specs)

Y.gpa<-gpagen(specs)  
fishshapes<-Y.gpa$coords
ref<-mshape(fishshapes)
#plotAllSpecimens(fishshapes)
plotRefToTarget(ref,ref,links=links)

### new code
PCA <- gm.prcomp(fishshapes)
PC <- PCA$x[,1]
preds <- shape.predictor(fishshapes, x= PC, Intercept = FALSE, 
                         pred1 = min(PC), pred2 = max(PC)) # PC 1 extremes, more technically

## pcmin and max
par(mfrow=c(2,2)) 
plot(PCA, pch=21, cex=1,bg="black")
plotRefToTarget(ref,ref,links=links)
plotRefToTarget(ref, preds$pred1, links = links)
mtext("PC1-Negative")
plotRefToTarget(ref, preds$pred2, links = links)
mtext("PC1-Positive")
par(mfrow=c(1,1)) 

####### Missing data plots
fish.colmin <- rep("black",dim(fishshapes)[[3]]); fish.colmin[1] ="red"
fish.szmin <- rep(1.0,dim(fishshapes)[[3]]); fish.szmin[1] =2
plot(PCA,pch=21, bg=fish.colmin, cex=fish.szmin)

fish.colmax <- rep("black",dim(fishshapes)[[3]]); fish.colmax[101] ="red"
fish.szmax <- rep(1.0,dim(fishshapes)[[3]]); fish.szmax[101] =2
plot(PCA,pch=21, bg=fish.colmax, cex=fish.szmax)

#create some missing data for TPS and regression methods  
shapes.missing<-fishshapes  #delete 1, 7, 15, 26 
shapes.missing[c(1,7,17,26),,1]<-NA
shapes.missing[c(1,7,17,26),,101]<-NA



########  2: Mean Substitution
shapes.mn<-shapes.missing
shapes.mn[c(1,7,17,26),,1]<-ref[c(1,7,17,26),]
shapes.mn[c(1,7,17,26),,101]<-ref[c(1,7,17,26),]


plotRefToTarget(ref,ref,links=links)
plotRefToTarget(fishshapes[,,1],fishshapes[,,1],links=links) #original
plotRefToTarget(ref,fishshapes[,,1],links=links)
plotRefToTarget(ref,shapes.mn[,,1],links=links)
as.matrix(dist(rbind(t(matrix(ref)),t(matrix(fishshapes[,,1])),t(matrix(shapes.mn[,,1])))))
  #both are quite different from reference, but VERY different from each other!

plotRefToTarget(ref,ref,links=links)
plotRefToTarget(fishshapes[,,101],fishshapes[,,101],links=links) #original
plotRefToTarget(ref,fishshapes[,,101],links=links)
plotRefToTarget(ref,shapes.mn[,,101],links=links)
as.matrix(dist(rbind(t(matrix(ref)),t(matrix(fishshapes[,,101])),t(matrix(shapes.mn[,,101])))))
#both are quite different from reference, but VERY different from each other!

#what's going on?  Show allometry
fish.col <- rep("black",dim(fishshapes)[[3]]); fish.col[c(1,101)] ="red"
fish.sz <- rep(1.0,dim(fishshapes)[[3]]); fish.sz[c(1,101)] =2

plotRefToTarget(ref,fishshapes[,,1],method="points", links=links)
plot(PCA,pch=21, bg=fish.col, cex=fish.sz)
plotRefToTarget(ref,fishshapes[,,101],method="points", links=links)


##### 3: TPS Interpolation
new.tps<-estimate.missing(shapes.missing,method="TPS")
plotRefToTarget(ref,ref,links=links)
plotRefToTarget(ref,fishshapes[,,1],links=links)
plotRefToTarget(ref,new.tps[,,1],links=links)
plotRefToTarget(fishshapes[,,1],new.tps[,,1],links=links)
as.matrix(dist(rbind(t(matrix(ref)),t(matrix(fishshapes[,,1])),t(matrix(new.tps[,,1])))))
#MUCH better!

plotRefToTarget(ref,fishshapes[,,101],links=links)
plotRefToTarget(ref,new.tps[,,101],links=links)
as.matrix(dist(rbind(t(matrix(ref)),t(matrix(fishshapes[,,101])),t(matrix(new.tps[,,101])))))
#MUCH better!


##### 4 Regression Interpolation
new.reg<-estimate.missing(shapes.missing,method="Reg")
new.reg<-gpagen(new.reg, print.progress=FALSE)$coords
plotRefToTarget(ref,fishshapes[,,1],links=links)
plotRefToTarget(ref,new.reg[,,1],links=links)
as.matrix(dist(rbind(t(matrix(ref)),t(matrix(fishshapes[,,1])),t(matrix(new.reg[,,1])))))
#Pretty good

plotRefToTarget(ref,fishshapes[,,101],links=links)
plotRefToTarget(ref,new.reg[,,101],links=links)
as.matrix(dist(rbind(t(matrix(ref)),t(matrix(fishshapes[,,101])),t(matrix(new.reg[,,101])))))
#Ditto!
