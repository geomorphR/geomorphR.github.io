library(geomorph)

setwd("/Users/antigoni/antigua/Courses/GM_Chile_2018/2019-Chile/Lectures/LectureData/03.superimposition")
liz <- readland.nts("lizards_LAT.nts")
links <- read.table("links.txt")

# VERSION 1 ####
# This plots several specimens at a time, but it is not as flexible and programmer-free as I want it to be :(. Scroll down for example.

plot.specimens <- function(A, links = NULL, group.col = NULL, ...){
  if (length(dim(A)) != 3) {
    stop("Data matrix not a 3D array (see 'arrayspecs').")
  }
  p <- dim(A)[1]; k <- dim(A)[2]; n <- dim(A)[3]
  for (i in 1:n){
    if (k == 2) {
      if(i == 1) {
        plot.new()
        plot.window(c(0.99, 1.01)*range(A[, 1, ]), c(0.99, 1.01)*range(A[, 2, ]), asp = 1)
      }
      if(!is.null(group.col)) {
        if (length(group.col)!=n){
          stop("Group colour detinition does not match n of individuals.")
        }
      } else {
        dots <- list(...)
        if(!is.null(dots$col)) dots$col <- "black"
        group.col <- rep(dots$col, n)
      }
      if(!is.null(links)){
        link.coords <- xy.coords(A[as.vector(t(links)), 1, i], A[as.vector(t(links)), 2, i])
        lapply(seq(1, length(link.coords$x), by = 2), function(l){
          segments(link.coords$x[l], link.coords$y[l], 
                   link.coords$x[l+1], link.coords$y[l+1], col = group.col[i], ...)
        }
        )
      }
      plot.xy(xy.coords(A[ , 1, i], A[ , 2, i]), type = "p", col = group.col[i], ...)
      
    }
  }
  
}

# A random factor:
fac1 <- sample(c(1:2), size = dim(liz)[3], replace = T)
plot.specimens(liz, links = links, group.col = ifelse(fac1==1, "red", "blue"))  # works
plot.specimens(liz, links = links, group.col = ifelse(fac1==1, "red", "blue"), pch = 23, 
               bg = "grey")  # limited
plot.specimens(liz, links = links, group.col = ifelse(fac1==1, "red", "blue"), pch = 23, 
               bg = ifelse(fac1==1, "red", "blue"))  # doesn´t work

# Can´t control each specimen individually :/. Limited to group.col for outline and links. AND it is still not fully elliptical, i.e., we don´t get rid of the function-specific plotting definitions, which was what I was trying to do. The issue here is that when you try to go elliptical directly on the parameters (e.g. bg in the last example), plot.xy loops over the points first, so it messes up my specimen loop setup, unless I put a specific parameter for that.


# VERSION 2 ####
# This plots a single specimen, providing full elliptical control of the plotting parameters. The user (if a bit capable with R) can then loop across specimens with specimen-specific plotting definitions of any kind. Then I used Mike´s layer approach to add the mean. Again, example below.

x <- liz [,,1]  
plot.single.specimen <- function(x, links = NULL, ...){
  if (length(dim(x)) != 2) {
    stop("Data should be a matrix with the xy(z) coordinates of a single specimen.")
  }
  p <- dim(x)[1]; k <- dim(x)[2]
  if (k == 2) {
    plot.new()
    plot.window(c(0.99, 1.01)*range(A[, 1, ]), c(0.99, 1.01)*range(A[, 2, ]), asp = 1)
    
    if(!is.null(group.col)) {
      if (length(group.col)!=n){
        stop("Group colour detinition does not match n of individuals.")
      }
    } else {
      dots <- list(...)
      if(!is.null(dots$col)) dots$col <- "black"
      group.col <- rep(dots$col, n)
    }
    if(!is.null(links)){
      link.coords <- xy.coords(A[as.vector(t(links)), 1, i], A[as.vector(t(links)), 2, i])
      lapply(seq(1, length(link.coords$x), by = 2), function(l){
        segments(link.coords$x[l], link.coords$y[l], 
                 link.coords$x[l+1], link.coords$y[l+1], col = group.col[i], ...)
      }
      )
    }
    plot.xy(xy.coords(A[ , 1, i], A[ , 2, i]), type = "p", col = group.col[i], ...)
    
  }
}








