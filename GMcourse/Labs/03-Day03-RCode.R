## Day 3 Material R-script

library(geomorph)

# GLM: ANOVA

data(pupfish) # GPA already performed

fit <- procD.lm(coords ~ Sex * Pop, 
                data = pupfish, iter = 999)
anova(fit)
fitm <- manova.update(fit, print.progress = FALSE)
summary(fitm, test = "Pillai")

#### Pairwise Comparisons: SIMPLE EXAMPLE

group <- interaction(pupfish$Pop, pupfish$Sex)
PW <- pairwise(fit, groups = group)
summary(PW)

# PCMS

#### PCM Data Prep
library(geiger)
plethtree <- read.tree('Data/plethtree.tre')

plethtree <- read.tree('Data/plethtree.tre')
plethland <- readland.tps('Data/PlethodonLand.tps',specID = "ID",
                          warnmsg = FALSE)
gps <- read.csv('Data/PlethGps.csv', header=TRUE, row.names=1)
Y.gpa <- gpagen(plethland, print.progress = FALSE)
M <- mshape(Y.gpa$coords)
size <- Y.gpa$Csize

shape <- Y.gpa$coords
shape.test <- treedata(phy = plethtree, data = two.d.array(shape), warnings = TRUE)
#no warnings. Everything matches in this case

data.matched <- treedata(phy = plethtree, data = gps, warnings=FALSE)
elev <- as.factor(data.matched$data); names(elev) <- row.names(data.matched$data)

gdf <- geomorph.data.frame(shape=shape, size=size,elev = elev, plethtree=plethtree)

links <- matrix(c(4,3,2,1,1,6,7,8,9,10,1,1,11,5,5,4,2,3,7,8,9,10,11,9,10,1),
                ncol=2,byrow=FALSE)
plot(ladderize(plethtree),edge.width=3)
axisPhylo(1)


### Phylogenetic Generalized Least Squares (PLGS): Linear Models

pgls.reg <- procD.pgls(f1 = shape ~ size, phy = plethtree, data = gdf, print.progress = FALSE)
summary(pgls.reg)

allom.plot <- plot(pgls.reg, type = "regression", predictor = gdf$size,
                   reg.type ="RegScore", pch=19, cex=1.5, xlab = "size") # make sure to have a predictor 
fit.line <- lm(allom.plot$RegScore ~ gdf$size)
abline(fit.line, col = "red")

### Phylogenetic Signal

PS.shape <- physignal(A=shape,phy=plethtree,iter=999, print.progress = FALSE)
summary(PS.shape)
plot(PS.shape)

### Phylogenetic Ordination

#### Phylomorphospace
PCA <- gm.prcomp(shape, phy=plethtree)
plot(PCA, phylo = TRUE, pch=21, bg=gdf$elev, cex=2, phylo.par = list(tip.labels = FALSE, node.labels = FALSE) )
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))

#### Phylogenetic PCA (pPCA): With GLS-centered residuals
pPCA <- gm.prcomp(shape, phy=plethtree, GLS = TRUE, transform = TRUE)
plot(pPCA, phylo = FALSE, pch=21, bg=gdf$elev, cex=2, phylo.par = list(tip.labels = FALSE, node.labels = FALSE) )
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))

#### Phylogenetic PCA (pPCA): With GLS-transformed residuals
pPCA2 <- gm.prcomp(shape, phy=plethtree, GLS = TRUE, transform = TRUE)
plot(pPCA2, phylo = TRUE, pch=21, bg=gdf$elev, cex=2, phylo.par = list(tip.labels = FALSE, node.labels = FALSE) )
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))

#### Phylogenetically-Aligned Components Analysis (PACA)
PACA <- gm.prcomp(shape, phy=plethtree, align.to.phy = TRUE)
plot(PACA, phylo = TRUE, pch=21, bg=gdf$elev, cex=2, phylo.par = list(tip.labels = FALSE, node.labels = FALSE) )
legend("topleft", pch=21, pt.bg = unique(gdf$elev), legend = levels(gdf$elev))

### Comparing Evolutionary Rate Models

ER<-compare.evol.rates(A=gdf$shape, phy=plethtree,gp=gdf$elev,iter=999, method = 'permutation',print.progress = FALSE)
summary(ER)
plot(ER)

EMR <- compare.multi.evol.rates(A=gdf$shape, phy=plethtree, gp=c(rep(1,5),rep(2,6)), print.progress = FALSE)
summary(EMR)
plot(EMR)

# Analysis of Symmetry

#### Matching Symmetry
  
data(mosquito)
Y.gpa <- gpagen(mosquito$wingshape, print.progress = FALSE)
plot(Y.gpa)
mosquito.sym <- bilat.symmetry(A = Y.gpa, ind = mosquito$ind, side=mosquito$side, 
                               object.sym = FALSE, print.progress = FALSE)
summary(mosquito.sym)

#### Object Symmetry

data('lizards')
Y.gpa <- gpagen(lizards$coords, print.progress = FALSE)
plot(Y.gpa)
lizard.sym <- bilat.symmetry(A = Y.gpa, ind = lizards$ind, replicate = lizards$rep,
                             object.sym = TRUE, land.pairs = lizards$lm.pairs, print.progress = FALSE)
summary(lizard.sym)
plot(lizard.sym, warpgrids = TRUE)

# Measurement Error

data(fishy)
ME2 <- measurement.error(
  Y = "coords",
  subjects = "subj",
  replicates = "reps",
  groups = "groups",
  data = fishy)

anova(ME2)
ICCstats(ME2, subjects = "Subjects", 
         with_in = "Systematic ME", groups = "groups")
P <- plot(ME2)
focusMEonSubjects(P, subjects = 18:20, shadow = TRUE)