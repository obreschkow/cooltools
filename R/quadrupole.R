#' Quadrupole tensor
#'
#' @description Compute the trace-free quadrupole tensor of a set of point masses
#'
#' @param x n-by-3 matrix (x[1:n,1:3]) specifying the 3D cartesian coordinates of n points
#' @param m n-vector with point masses, or signle scalar giving a uniform mass for all points (defaults is unity)
#'
#' @return Returns a 3-by-3 symmetric matrix
#'
#' @author Danail Obreschkow
#'
#' @export

quadrupole = function(x,m=1) {

  if (length(x)==3) x = array(x,c(1,3)) # to make sure that x is an array

  Q = array(NA,c(3,3))
  Q[1,1] = sum(m*(2*x[,1]^2-x[,2]^2-x[,3]^2))
  Q[2,2] = sum(m*(2*x[,2]^2-x[,3]^2-x[,1]^2))
  Q[3,3] = sum(m*(2*x[,3]^2-x[,1]^2-x[,2]^2))
  Q[1,2] = sum(m*(3*x[,1]*x[,2]))
  Q[2,3] = sum(m*(3*x[,2]*x[,3]))
  Q[3,1] = sum(m*(3*x[,3]*x[,1]))
  Q[2,1] = Q[1,2]
  Q[3,2] = Q[2,3]
  Q[1,3] = Q[3,1]

  return(Q)

}
