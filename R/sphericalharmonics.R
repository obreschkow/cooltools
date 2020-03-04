#' Spherical Harmonics
#'
#' @description Evaluates complex-valued spherical harmonics Y in the Condon-Shortley phase convention.
#'
#' @param l degree of the spherical harmonic (0,1,2,3,4) = (monopole, dipole, quadrupole, octupole, hexadecapole)
#' @param m sub-order of the spherical harmonic (-l,-l+1,...,+l)
#' @param x n-by-3 matrix [1:n,1:3] specifying the 3D coordinates of n points; if x is given, theta and phi cannot be specified.
#' @param theta n-vector of azimuth angles from 0 to pi; if theta is given, phi must also be given, but x must not be given.
#' @param phi n-vector of longitude angles from 0 to 2*pi; if phi is given, theta must also be given, but x must not be given.
#'
#' @return Returns an n-vector of the spherical harmonics; for points x=c(0,0,0), a value of 0 is returned
#'
#' @author Danail Obreschkow
#'
#' @examples
#' ## Check orthonormalization of all spherical harmonics up to 4th order
#'
#' # make indices l and m up to 4th order
#' l = c(0,rep(1,3),rep(2,5),rep(3,7),rep(4,9))
#' m = c(0,seq(-1,1),seq(-2,2),seq(-3,3),seq(-4,4))
#'
#' # check orthonormalization for all pairs
#' for (i in seq(25)) {
#'   for (j in seq(25)) {
#'
#'     # compute scalar product
#'     f = function(theta,phi) {
#'       Yi = sphericalharmonics(l[i],m[i],theta=theta,phi=phi)
#'       Yj = sphericalharmonics(l[j],m[j],theta=theta,phi=phi)
#'       return(Re(Yi*Conj(Yj))*sin(theta))
#'     }
#'     g = Vectorize(function(phi) integrate(f,0,pi,phi)$value)
#'     scalar.product = integrate(g,0,2*pi)$value
#'
#'     # compare scalar product to expected value
#'     ok = abs(scalar.product-(i==j))<1e-6
#'     cat(sprintf('(l=%1d,m=%+1d|=%1d,m=%+1d)=%5.3f  %s\n',l[i],m[i],l[j],m[j],
#'                 scalar.product+1e-10,ifelse(ok,'ok','wrong')))
#'   }
#' }
#'
#' @export

sphericalharmonics = function(l,m,x=NULL,theta=NULL,phi=NULL) {

  if (!is.null(x)) {

    # convert cartesian to spherical coordinates
    if (!is.null(theta) | !is.null(phi)) stop('specify either x or (theta,phi).')
    if (!is.array(x)) x = array(x,c(1,3))
    r = sqrt(x[,1]^2+x[,2]^2+x[,3]^2)
    phi = atan2(x[,2],x[,1])
    theta = acos(x[,3]/r)
    n = length(r)
    zeros = r==0

  } else {

    # check spherical coordinates
    if (is.null(theta) | is.null(phi)) stop('specify either x or (theta,phi).')
    n = length(theta)
    zeros = NULL

  }

  if (l==0) { # monopole

    if (m!=0) stop("m must be equal to -l,-l+1,...,+l")
    Y = rep(1/2/sqrt(pi),n)

  } else if (l==1) { # dipole

    if (m==-1) {
      Y = 0.5*sqrt(3/2/pi)*sin(theta)*exp(-1i*phi)
    } else if (m==0) {
      Y = 0.5*sqrt(3/pi)*cos(theta)
    } else if (m==1) {
      Y = -0.5*sqrt(3/2/pi)*sin(theta)*exp(1i*phi)
    } else {
      stop("m must be equal to -l,-l+1,...,+l")
    }

  } else if (l==2) { # quadrupole

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

  } else if (l==3) { # octupole

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

  } else if (l==4) { # hexadecapole}

    if (m==-4) {
      Y = (3/16)*sqrt(35/2/pi)*exp(-4i*phi)*sin(theta)^4
    } else if (m==-3) {
      Y = (3/8)*sqrt(35/pi)*exp(-3i*phi)*sin(theta)^3*cos(theta)
    } else if (m==-2) {
      Y = (3/8)*sqrt(5/2/pi)*exp(-2i*phi)*sin(theta)^2*(7*cos(theta)^2-1)
    } else if (m==-1) {
      Y = (3/8)*sqrt(5/pi)*exp(-1i*phi)*sin(theta)*(7*cos(theta)^3-3*cos(theta))
    } else if (m==0) {
      Y = (3/16)*sqrt(1/pi)*(35*cos(theta)^4-30*cos(theta)^2+3)
    } else if (m==1) {
      Y = -(3/8)*sqrt(5/pi)*exp(1i*phi)*sin(theta)*(7*cos(theta)^3-3*cos(theta))
    } else if (m==2) {
      Y = (3/8)*sqrt(5/2/pi)*exp(2i*phi)*sin(theta)^2*(7*cos(theta)^2-1)
    } else if (m==3) {
      Y = -(3/8)*sqrt(35/pi)*exp(3i*phi)*sin(theta)^3*cos(theta)
    } else if (m==4) {
      Y = (3/16)*sqrt(35/2/pi)*exp(4i*phi)*sin(theta)^4
    } else {
      stop("m must be equal to -l,-l+1,...,+l")
    }

  } else {

    stop('l must be equal to 0,1,2,3,4 in this implementation')

  }

  Y[zeros] = 0

  return(Y)

}
