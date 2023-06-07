#' Second moment tensor
#'
#' @description Compute the tensor of second moments of a set of point masses
#'
#' @param x n-by-3 matrix (x[1:n,1:3]) specifying the 3D Cartesian coordinates of n points
#' @param m n-vector with point masses, or single scalar giving a uniform mass for all points (default is unity)
#'
#' @return Returns a 3-by-3 symmetric matrix
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{inertia}}, \code{\link{quadrupole}}
#'
#' @example
#' # Make a randomly oriented ellipsoid of semi-axes a=2.1, b=1.73, c=0.8
#' x = t(t(fibonaccisphere(1e4))*c(2.1,1.73,0.8))
#' x = x%*%rotation3(c(0.3,1.64,2.31))
#'
#' # Recover lengths of semi-axes from eigenvalues of second moment tensor
#' M = moments(x,m=1/dim(x)[1])
#' v = sqrt(3*eigen(M)$values)
#' print(v)
#'
#' @export

moments = function(x,m=1) {

  if (length(x)==3) x = array(x,c(1,3)) # to make sure that x is an array

  M = array(NA,c(3,3))
  M[1,1] = sum(m*x[,1]^2)
  M[2,2] = sum(m*x[,2]^2)
  M[3,3] = sum(m*x[,3]^2)
  M[1,2] = M[2,1] = sum(m*x[,1]*x[,2])
  M[2,3] = M[3,2] = sum(m*x[,2]*x[,3])
  M[3,1] = M[1,3] = sum(m*x[,3]*x[,1])

  return(M)

}

