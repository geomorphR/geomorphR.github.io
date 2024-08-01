# This R script contains the concatenated code from the day 1 tutorials.
# This is what you should work through during lab, using your own data.

##### Digitizing in StereoMorph
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
               curves.ref = "Data/example.curves.txt")

# Read Data

shapes <- readShapes("Shapes") # Steromorph function.  Turns image txt files into a list
shapesGM <- readland.shapes(shapes, nCurvePts = 10) # geomorph conversion function

# GPA

Y.gpa <- gpagen(shapesGM)
plot(Y.gpa)
