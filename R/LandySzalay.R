#' Two-point correlation estimation
#'
#' @description Evaluates the Landy-Szalay (1993) estimator of the two-point correlation function of a point set D given a random comparison set R. The two point sets D and R can be made of different numbers of points, as the pair-counts are automatically normalized according to the number of points. In fact, it is often preferable to make the R set larger to reduce the R-related shot noise in the two-point estimator.
#'
#' @param D n-element vector or n-by-d matrix of d-dimensional positions of the data points
#' @param R m-element vector or m-by-d matrix of d-dimensional positions of the random comparison points
#' @param dr bin size for the evaluation of the two-point correlation function
#' @param cpp logical flag; if set to TRUE (default) a fast implementation in C++ is used to count the point-pairs in distance bins, otherwise the counting is performed less efficiently in R.
#'
#' @return Returns a list with the two-point statistics of the data points:
#' \item{r}{vector with the mid-points of the distance bins for which the two-point correlation function has been evaluated.}
#' \item{xi}{values of the two-point correlation function at the distances r.}
#' \item{err}{Poisson errors of xi.}
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{paircount}}
#'
#' @export

landyszalay = function(D,R,dr=0.1,cpp=TRUE) {

  # convert vectors (1D) and data frames into matrices
  D = as.matrix(D)
  R = as.matrix(R)
  if (dim(D)[2]!=dim(R)[2]) stop('D and R must have the same number of columns.')

  # determine size of data
  nD = dim(D)[1]
  nR = dim(R)[1]
  d = dim(D)[2]
  if (dim(R)[2]!=d) stop('D and R must have the same number of rows.')

  # determine maximal possible distance
  rmax = 0
  for (i in seq(d)) {
    minval = min(min(D[,i]),min(R[,i]))
    maxval = max(max(D[,i]),max(R[,i]))
    rmax = rmax+(maxval-minval)^2
  }
  rmax = sqrt(rmax)

  # count pairs
  DD = paircount(D,dr=dr,rmax=rmax,cpp=cpp)$n
  RR = paircount(R,dr=dr,rmax=rmax,cpp=cpp)$n
  DR = paircount(D,R,dr=dr,rmax=rmax,cpp=cpp)$n

  # crop tailing zeros
  n = max(which(DD!=0))
  DD = DD[1:n]
  RR = RR[1:n]
  DR = DR[1:n]

  # compute L-S estimator
  nDD = nD*(nD-1)/2
  nRR = nR*(nR-1)/2
  nDR = nD*nR
  xi = (DD/nDD-2*DR/nDR)/RR*nRR+1
  err = sqrt(DD*(nRR/nDD/RR)^2+4*DR*(nRR/nDR/RR)^2+RR*((DD/nDD-2*DR/nDR)/RR^2*nRR)^2)
  xi[RR==0 | DD==0] = err[RR==0 | DD==0] = NA

  # return results
  return(data.frame(r=seq(0,n-1)*dr, xi=xi, err=err))

}

