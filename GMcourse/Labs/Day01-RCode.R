# This R script contains the concatenated code from the day 1 tutorials.
# This is what you should work through during lab, using your own data.

##### 1: Digitizing in StereoMorph
install.packages("StereoMorph")
library(StereoMorph)
library(geomorph)

# Landmark digtizing: see Tutorial for additional instructions
digitizeImages(image.file='Data/Fish-Images', shapes.file='Data/Fish-Shapes',
               landmarks.ref=paste("LM", c(1:5), sep=""))

## Scaling: See Tutorial for instructions

# Read Data

shapes <- readShapes("Data/Fish-Shapes") # Steromorph function.  Turns image txt files into a list
shapesGM <- readland.shapes(shapes) # geomorph conversion function

# GPA (more below)

Y.gpa <- gpagen(shapesGM)
plot(Y.gpa)

# Digitizing a curve (add to existing LMs)

digitizeImages(image.file='Data/Fish-Images', shapes.file='Data/Fish-Shapes',
               landmarks.ref=paste("LM", c(1:5), sep=""),
               curves.ref = "example.curves.txt")

# Read Data

shapes <- readShapes("Shapes") # Steromorph function.  Turns image txt files into a list
shapesGM <- readland.shapes(shapes, nCurvePts = 10) # geomorph conversion function

# GPA

Y.gpa <- gpagen(shapesGM)
plot(Y.gpa)

##### 2: Read Data: For reading other types of data, see tutorial for instructions


##### 3: Data Pre-Processing

## 3A: Estimate Missing Landmarks

#### build some missing data (EXAMPLE ONLY)
data(plethodon)
Y.gpa <- gpagen(plethodon$land, print.progress = FALSE)
plethland<-Y.gpa$coords
plethland[3,,2]<-plethland[8,,2]<-NA  #create missing landmarks
plethland[3,,5]<-plethland[8,,5]<-plethland[9,,5]<-NA  
plethland[3,,10]<-NA  

# Estimate via TPS or Regression
new.tps <- estimate.missing(plethland,method="TPS")
new.reg <- estimate.missing(plethland,method="Reg")

plotRefToTarget(Y.gpa$coords[,,3],new.tps[,,3],links=plethodon$links, mag=3)
plotRefToTarget(Y.gpa$coords[,,3],new.reg[,,3],links=plethodon$links, mag=3)

## 3B: Check for Outliers (EXAMPLE ONLY)
  newland <- plethodon$land
  newland[c(1,8),,2] <- newland[c(8,1),,2] #digitized out of order
  newland[c(3,11),,26] <- newland[c(11,3),,2]

Y <- gpagen(newland, print.progress = FALSE) 
plotOutliers(Y$coords, inspect.outliers = F)
plotOutliers(Y$coords, inspect.outliers = T)

##### 4: Generalized Procrustes Analysis: GPA

data(plethodon)
pleth.gpa <- gpagen(plethodon$land, print.progress = F)
summary(pleth.gpa)

plot(pleth.gpa)
plotAllSpecimens(pleth.gpa$coords, links = plethodon$links)
