#Code and analyses for 1.1-Reading Data

## ----prelims----

options(rgl.useNULL = TRUE)
library(rgl)
library(geomorph)


## ----stereomorph----
library(StereoMorph)

myShapes <- readShapes("../Tutorials/Data-figures/Shapes") 
mydata <- readland.shapes(myShapes)
#plot lmks
mydata$landmarks$Fish1

## ----tps----

tpsdata <- readland.tps("../Tutorials/Data-figures/salamanders.tps")
str(tpsdata)
dim(tpsdata)  
tpsdata[,,1]

tpsdata <- readland.tps("../Tutorials/Data-figures/salamanders.tps", specID="imageID") # Specify specimen labels
str(tpsdata)
tpsdata[,,1:2]

## ----nts----

ntsdata <- readland.nts("../Tutorials/Data-figures/RATS.nts")
str(ntsdata)
ntsdata[,,1]

## ----mtps----

mtpsdata <- readmulti.tps(c("../Tutorials/Data-figures/test1.tps", "../Tutorials/Data-figures/test2.tps"))
str(mtpsdata)
mtpsdata[,,1]

## ----mnts----
mtpsdata <- readmulti.nts(c("../Tutorials/Data-figures/Pleth1.nts", 
                            "../Tutorials/Data-figures/Pleth2.nts", 
                            "../Tutorials/Data-figures/Pleth3.nts"))
str(mtpsdata)
mtpsdata[,,1]

## ----morphologika----

morphdata <- read.morphologika("../Tutorials/Data-figures/mophologikaexample.txt")
str(morphdata)
dim(morphdata$coords)
morphdata$coords[,,1]

## ----plyfile----

mandible <- read.ply("../Tutorials/Data-figures/Mandible.ply", ShowSpecimen = TRUE)
rglwidget()


## ----fromtext----

Salamanders <- read.csv("../Tutorials/Data-figures/salamanders.csv", header=FALSE)
library(ape)
tree <- read.tree("../Tutorials/Data-figures/plethtree.tre")
plot(tree)