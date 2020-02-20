#' Spherical Harmonics
#'
#' @description Evaluates complex-valued spherical harmonics in the Condon-Shortley phase convention.
#'
#' @param l degree of the spherical harmonic (0,1,2,3)
#' @param m sub-order of the spherical harmonic (-l,-l+1,...,+l)
#' @param x n-by-3 matrix [1:n,1:3] specifying the 3D coordinates of n points; if x is given, theta and phi cannot be specified.
#' @param theta n-vector of azimuth angles from 0 to pi; if theta is given, phi must also be given, but x must not be given.
#' @param phi n-vector of longitude angles from 0 to 2*pi; if phi is given, theta must also be given, but x must not be given.
#'
#' @return Returns an n vectors of the spherical harmonics.
#'
#' @author Danail Obreschkow
#'
#' @export

sphericalY = function(l,m,x=NULL,theta=NULL,phi=NULL) {

  if (!is.null(x)) {
    if (!is.null(theta) | !is.null(phi)) stop('specify either x or (theta,phi).')
    if (!is.array(x)) x = array(x,c(1,3))
    r = sqrt(x[,1]^2+x[,2]^2+x[,3]^2)
    phi = atan2(x[,2],x[,1])
    theta = acos(x[,3]/r)
  } else {
    if (is.null(theta) | is.null(phi)) stop('specify either x or (theta,phi).')
  }

  if (l==0) {

    if (m!=0) stop("m must be equal to -l,-l+1,...,+l")
    Y = rep(1/2/sqrt(pi),dim(x)[1])

  } else if (l==1) {

    if (m==-1) {
      Y = 0.5*sqrt(3/2/pi)*sin(theta)*exp(-1i*phi)
    } else if (m==0) {
      Y = 0.5*sqrt(3/pi)*cos(theta)
    } else if (m==1) {
      Y = -0.5*sqrt(3/2/pi)*sin(theta)*exp(1i*phi)
    } else {
      stop("m must be equal to -l,-l+1,...,+l")
    }

  } else if (l==2) {

    if (m==-2) {
      Y = 0.25*sqrt(15/2/pi)*sin(theta)^2*exp(-2i*phi)
    } else if (m==-1) {
      Y = 0.5*sqrt(15/2/pi)*sin(theta)*cos(theta)*exp(-1i*phi)
    } else if (m==0) {
      Y = 0.25*sqrt(5/pi)*(3*cos(theta)^2-1)
    } else if (m==1) {
      Y = -0.5*sqrt(15/2/pi)*sin(theta)*cos(theta)*exp(1i*phi)
    } else if (m==2) {
      Y = 0.25*sqrt(15/2/pi)*sin(theta)^2*exp(2i*phi)
    } else {
      stop("m must be equal to -l,-l+1,...,+l")
    }

  } else if (l==3) {

    if (m==-3) {
      Y = 0.125*sqrt(35/pi)*exp(-3i*phi)*sin(theta)^3
    } else if (m==-2) {
      Y = 0.25*sqrt(105/2/pi)*exp(-2i*phi)*sin(theta)^2*cos(theta)
    } else if (m==-1) {
      Y = 0.125*sqrt(21/pi)*exp(-1i*phi)*sin(theta)*(5*cos(theta)^2-1)
    } else if (m==0) {
      Y = 0.25*sqrt(7/pi)*(5*cos(theta)^3-3*cos(theta))
    } else if (m==1) {
      Y = -0.125*sqrt(21/pi)*exp(1i*phi)*sin(theta)*(5*cos(theta)^2-1)
    } else if (m==2) {
      Y = 0.25*sqrt(105/2/pi)*exp(2i*phi)*sin(theta)^2*cos(theta)
    } else if (m==3) {
      Y = -0.125*sqrt(35/pi)*exp(3i*phi)*sin(theta)^3
    } else {
      stop("m must be equal to -l,-l+1,...,+l")
    }

  } else {

    stop('l must be equal to 0,1,2,3 in this implementation')

  }

  Y[r==0] = 0

  return(Y)

}
