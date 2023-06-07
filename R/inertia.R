#' Inertia tensor
#'
#' @description Computes the symmetric tensor of moments of inertia
#'
#' @param x n-by-3 matrix (x[1:n,1:3]) specifying the 3D Cartesian coordinates of n points
#' @param m n-vector with point masses, or single scalar giving a uniform mass for all points (default is unity)
#'
#' @return Returns a 3-by-3 symmetric matrix
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{quadrupole}}, \code{\link{moments}}
#'
#' @export

inertia = function(x,m=1) {

  if (length(x)==3) x = array(x,c(1,3)) # to make sure that x is an array

  I = array(NA,c(3,3))
  I[1,1] = sum(m*(x[,2]^2+x[,3]^2))
  I[2,2] = sum(m*(x[,3]^2+x[,1]^2))
  I[3,3] = sum(m*(x[,1]^2+x[,2]^2))
  I[1,2] = I[2,1] = -sum(m*x[,1]*x[,2])
  I[2,3] = I[3,2] = -sum(m*x[,2]*x[,3])
  I[3,1] = I[1,3] = -sum(m*x[,3]*x[,1])

  return(I)

}
