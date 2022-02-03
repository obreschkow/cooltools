#' Count the number of point-pairs in distance bins
#'
#' @description Count the number of point-pairs in equally spaced distances bins. Code works in any dimension. If only one point set is provided, the distances of this point set with itself are used (counting each pairs only once, i.e. only ij, not ji).
#'
#' @param x n-element vector or n-by-d matrix of d-dimensional positions of data points
#' @param y optional m-element vector or m-by-d matrix of d-dimensional positions of a second point set
#' @param dr distance bin size
#' @param rmax maximum distance to be considered
#' @param cpp logical flag; if set to TRUE (default) a fast implementation in C++ is used, otherwise the counting is performed less efficiently in R.
#'
#' @return Returns a list with two vectors
#' \item{r}{vector with the mid-points of the distance bins.}
#' \item{n}{number of pairs in the distance bin.}
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{landyszalay}}
#'
#' @export

paircount = function(x, y=NULL, dr, rmax, cpp=TRUE) {

  nr = round(rmax/dr)+1

  if (is.null(y)) {

    x = as.matrix(x)

    if (cpp) {
      if (dim(as.matrix(x))[2]==1) {
        count = paircountxx1d(x,dr,rmax)$count
      } else {
        count = paircountxx(x,dr,rmax)$count
      }
    } else {
      nx = dim(x)[1]
      count = c(nx,rep(0,nr-1))
      for (i in seq(nx-1)) {
        for (j in seq(i+1,nx)) {
          d = sqrt(sum((x[i,]-x[j,])^2))
          if (d<=rmax) {
            index = round(d/dr)+1
            count[index] = count[index]+1
          }
        }
      }
    }

  } else {

    x = as.matrix(x)
    y = as.matrix(y)

    if (dim(x)[2]!=dim(y)[2]) stop('x and y must have the same number of columns.')

    if (cpp) {
      if (dim(as.matrix(x))[2]==1) {
        count = paircountxy1d(x,y,dr,rmax)$count
      } else {
        count = paircountxy(x,y,dr,rmax)$count
      }
    } else {
      nx = dim(x)[1]
      ny = dim(y)[1]
      count = rep(0,nr)
      for (i in seq(nx)) {
        for (j in seq(ny)) {
          d = sqrt(sum((x[i,]-y[j,])^2))
          if (d<=rmax) {
            index = round(d/dr)+1
            count[index] = count[index]+1
          }
        }
      }
    }
  }

  return(data.frame(r=seq(0,nr-1)*dr, n=count))

}
