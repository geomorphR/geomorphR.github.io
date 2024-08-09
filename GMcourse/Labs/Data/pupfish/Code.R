library(geomorph)
mydata <- readland.tps('pupfish.tps')
plotAllSpecimens(mydata)
plotAllSpecimens(mydata[,,1:2], label = TRUE) #to show the points on the curves

#building curves matrix (there are several curves)
c1 <- c(8 , 34, 35, 36, 37, 38, 9)
c2 <- c(39, 40, 41, 42, 43, 44, 45, 46, 47)
c3 <- c(4, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 5)
c4 <- c(5, 24, 25, 26, 27, 28, 29, 30, 6)
c5 <- c(6, 31, 32, 33, 7)
c6 <- c(49, 50, 51, 52, 53, 54, 55, 56 )

sliders <- rbind(define.sliders(c1,5), define.sliders(c2,7),
                 define.sliders(c3,13), define.sliders(c4,7),
                 define.sliders(c5,3), define.sliders(c6,6),
                 c(55,56,49),c(56,49,50))

## NOW DO GPA with points and semilandmarks on curves
Y.gpa <- gpagen(mydata, curves = sliders, ProcD = TRUE)

plotAllSpecimens(Y.gpa$coords)
