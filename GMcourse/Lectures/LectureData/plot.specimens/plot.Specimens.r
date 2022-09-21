plot.specimens <- function(A, links = NULL, ...){
  if (length(dim(A)) != 3) {
    stop("Data matrix not a 3D array (see 'arrayspecs').")
  }
  p <- dim(A)[1]; k <- dim(A)[2]; n <- dim(A)[3]
  dots <- list(...)
  if(!all(unlist(lapply(dots, length)) %in% c(1, n))){
    stop("For plot.specimens, all point plotting arguments must be equal in length to the number of specimens or just one value\n")
  }
  if(k == 2) {
    for (i in 1:n) {
      if(i == 1) {
        plot.new()
        plot.window(c(0.99, 1.01)*range(A[, 1, ]), c(0.99, 1.01)*range(A[, 2, ]), asp = 1)
      }
      
      dots.i <- dots
      if(any(unlist(lapply(dots.i, length))==n)){
        for (u in which(unlist(lapply(dots.i, length))==n)){
          dots.i[u] <- dots.i[[u]][i]
        }
      }
      
      if(!is.null(links)){
        dots.i$xy <- xy.coords(A[as.vector(t(links)), 1, i], A[as.vector(t(links)), 2, i])
        dots.i$type <- "b"
        do.call(plot.xy, args = c(dots.i))
      } else {
        dots.i$xy <- xy.coords(A[ , 1, i], A[ , 2, i])
        dots.i$type <- "p"
        do.call(plot.xy, args = c(dots.i))
      }
    }
    
  }
  out <- list(mshape = mshape(A), links = links)
  class(out) <- "plot.specimens"
  out
}

add.mshape <- function(SP, ...){
  msh <- SP$mshape
  links <- SP$links
  dots <- list(...)
  if(is.null(dots$cex)) dots$cex <- 1.5
  if(is.null(dots$lwd)) dots$lwd <- 2
  
  if(dim(msh)[2] == 2){
    if(!is.null(links)){
      dots$xy <- xy.coords(msh[as.vector(t(links)), 1], msh[as.vector(t(links)), 2])
      dots$type <- "b"
      do.call(plot.xy, args = c(dots))
    } else {
      dots$xy <- xy.coords(msh[ , 1], msh[ , 2])
      dots$type <- "p"
      do.call(plot.xy, args = c(dots))
    }
  }
}