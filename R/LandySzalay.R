#' Two-point correlation estimation
#'
#' @description Evaluates the Landy-Szalay (1993) estimator of the two-point correlation function of a point set D given a random comparison set R.
#'
#' @param D n-element vector or n-by-d matrix of d-dimensional positions of data points
#' @param R m-element vector or m-by-d matrix of d-dimensional positions of random comparison points
#' @param dr bin size for the evaulation of the two-point correlation function
#'
#' @return Returns a list with the two-point statistics of the data points:
#' \item{r}{vector with the mid-points of the distance bins for which the two-point correlation function has been evaluated.}
#' \item{xi}{values of the two-point correlation function at the distances r.}
#' \item{err}{Poisson errors of xi.}
#' \item{DD}{normalised DD pair-counts (without zero-distance pairs).}
#' \item{RR}{normalised RR pair-counts (without zero-distance pairs).}
#' \item{DR}{normalised DR pair-counts (without zero-distance pairs).}
#'
#' @author Danail Obreschkow
#'
#' @export

LandySzalay = function(D,R,dr=0.1) {

  # D: n-by-d matrix with d-dimensional positions of the observed point set
  # R: m-by-d matrix with d-dimensional positions of the random point set

  # convert vectors (1D) and data frames into matrices
  D = as.matrix(D)
  R = as.matrix(R)

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
  DD = .paircount(D,dr=dr,rmax=rmax)
  RR = .paircount(R,dr=dr,rmax=rmax)
  DR = .paircount(D,R,dr=dr,rmax=rmax)

  # shell radii
  nc = length(DD)
  r.min = seq(0,nc-1)*dr
  r.max = seq(1,nc)*dr
  r.mid = (r.min+r.max)/2

  # compute L-S estimator
  nDD = nD^2/2
  nRR = nR^2/2
  nDR = nD*nR
  xi = (DD/nDD-2*DR/nDR)/RR*nRR+1
  err = sqrt(DD*(nRR/nDD/RR)^2+4*DR*(nRR/nDR/RR)^2+RR*((DD/nDD-2*DR/nDR)/RR^2*nRR)^2)
  xi[RR==0 | DD==0] = err[RR==0 | DD==0] = NA

  # return results
  return(list(r=r.mid, xi=xi, err=err, DD=DD/nDD, RR=RR/nRR, DR=DR/nDR))

}

.paircount = function(x, y=NULL, dr, rmax) {

  nx = dim(x)[1]
  nr = ceiling(rmax/dr)
  count = array(0,nr)

  if (is.null(y)) {
    for (i in seq(nx-1)) {
      for (j in seq(i+1,nx)) {
        d = sqrt(sum((x[i,]-x[j,])^2))
        if (d>0 & d<=rmax) {
          index = ceiling(d/dr)
          count[index] = count[index]+1
        }
      }
    }
  } else {
    ny = dim(y)[1]
    for (i in seq(nx)) {
      for (j in seq(ny)) {
        d = sqrt(sum((x[i,]-y[j,])^2))
        if (d>0 & d<=rmax) {
          index = ceiling(d/dr)
          count[index] = count[index]+1
        }
      }
    }
  }

  return(count)
}
