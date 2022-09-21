library(geomorph)

liz <- readland.nts("lizards_LAT.nts")
links <- read.table("links.txt")

y.gpa <- gpagen(liz, print.progress = FALSE)
ref <- y.gpa$consensus * mean(y.gpa$Csize)
newdat <- hummingbirds$land[,,1:2]; newdat[,,1] <- ref

#Center-Scale: 
dat.cs <- lapply(1:2, function(j) geomorph:::center.scale(newdat[,,j])$coords)
dat.csplot <- simplify2array(dat.cs)

#rotate
dat.rot <- geomorph:::apply.pPsup(dat.cs[[1]],dat.cs)
dat.gpa <- simplify2array(dat.rot)

#slide


# PLOT ####
# Trying to make functions for plotting specimens with individual links and plotting options
# Would be a future modification to plotAllSpecimens, which we need to make through plot.xy
# But my head has no more energy for solving this today :/
A <- liz
plot.specimens <- function(A, links = NULL, link.col = "grey", ...){
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
      if(!is.null(links)){
        link.coords <- xy.coords(A[as.vector(t(links)), 1, i], A[as.vector(t(links)), 2, i])
        if(length(link.col)==1) link.col <- rep(link.col, n)
        lapply(seq(1, length(link.coords$x), by = 2), function(l){
          segments(link.coords$x[l], link.coords$y[l], 
                   link.coords$x[l+1], link.coords$y[l+1], col = link.col[i])
        }
        )
      }
      plot.xy(xy.coords(A[ , 1, i], A[ , 2, i]), type = "p", ...)
      
      }
    }
}


plot.one.specimen <- function(x, links = links, ...){
  dots <- list(...)
  if(is.null(dots$col)) dots$col <- "grey"
    if (k == 2) {
      if(!is.null(links)){
        link.coords <- xy.coords(x[as.vector(t(links)), 1], x[as.vector(t(links)), 2])
        if(length(link.col)==1) link.col <- rep(link.col, n)
        lapply(seq(1, length(link.coords$x), by = 2), function(l){
          segments(link.coords$x[l], link.coords$y[l], 
                   link.coords$x[l+1], link.coords$y[l+1], col = dots$col)
        }
        )
      }
      plot.xy(xy.coords(x[ , 1], x[ , 2]), type = "p", ...)
    }
}

plot.many.specimens <- function(A, links = NULL, ...){
  if (length(dim(A)) != 3) {
    stop("Data matrix not a 3D array (see 'arrayspecs').")
  }
  p <- dim(A)[1]; k <- dim(A)[2]; n <- dim(A)[3]
  for (i in 1:n){
      if(i == 1) {
        plot.new()
        plot.window(c(0.99, 1.01)*range(A[, 1, ]), c(0.99, 1.01)*range(A[, 2, ]), asp = 1)
      }
    plot.one.specimen(A[,,i], links = links, ...)
  }
}

plot.many.specimens(liz, links = links)

      
# A random factor:
fac1 <- sample(c(1:2), size = dim(liz)[3], replace = T)
plot.specimens(liz, links = links, col = ifelse(fac1==1, "red", "blue"),
               link.col = ifelse(fac1==1, "red", "blue"))  
  
  
  