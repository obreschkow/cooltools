#' Spherical Harmonics
#'
#' @importFrom stats integrate
#'
#' @description Evaluates spherical harmonics Y, either in the real-valued or complex-valued basis.
#'
#' @param l degree of the spherical harmonic, accurate to about l=500; (0=monopole, 1=dipole, 2=quadrupole, 3=octupole, 4=hexadecapole,...)
#' @param m order of the spherical harmonic (-l,-l+1,...,+l)
#' @param x either an n-by-2 matrix specifying the polar angle \code{theta} (0...pi) and azimuthal angle \code{phi} (0...2*pi); or an n-by-3 matrix specifying the 3D coordinates of n vectors (whose normalization is irrelevant).
#' @param basis a string specifying the type of spherical harmonics; this has to be either "complex" for the standard complex-valued harmonics with Condon-Shortley phase convention, or "real" (default) for the standard real-valued harmonics.
#'
#' @return Returns an n-vector of the spherical harmonics; for points x=c(0,0,0), a value of 0 is returned
#'
#' @author Danail Obreschkow
#'
#' @examples
#' ## Check orthonormalization of all spherical harmonics up to 3rd degree
#'
#' # make indices l and m up to 3rd degree
#' l = c(0,rep(1,3),rep(2,5),rep(3,7))
#' m = c(0,seq(-1,1),seq(-2,2),seq(-3,3))
#'
#' # check orthonormalization for all pairs
#' for (i in seq(16)) {
#'   for (j in seq(16)) {
#'
#'     # compute scalar product
#'     f = function(theta,phi) {
#'       Yi = sphericalharmonics(l[i],m[i],cbind(theta,phi))
#'       Yj = sphericalharmonics(l[j],m[j],cbind(theta,phi))
#'       return(Re(Yi*Conj(Yj))*sin(theta))
#'     }
#'     g = Vectorize(function(phi) integrate(f,0,pi,phi)$value)
#'     scalar.product = integrate(g,0,2*pi)$value
#'
#'     # compare scalar product to expected value
#'     ok = abs(scalar.product-(i==j))<1e-6
#'     cat(sprintf('(l=%1d,m=%+1d|l=%1d,m=%+1d)=%5.3f  %s\n',l[i],m[i],l[j],m[j],
#'                 scalar.product+1e-10,ifelse(ok,'ok','wrong')))
#'   }
#' }
#'
#' @export

sphericalharmonics = function(l, m, x, basis='real') {

  # input checks
  if (length(l)!=1) stop('l must be a single integer')
  if (l<0) stop('l must be a non-negative integer')
  if (length(m)!=1) stop('m must be a single integer')
  if (round(l)!=l) stop('l must be an integer')
  if (round(m)!=m) stop('m must be an integer')
  if (abs(m)>l) stop("m must be equal to -l,-l+1,...,+l")
  if (length(dim(x))!=2) {
    if (length(x)==2 | length(x)==3) {
      x = rbind(x)
    } else {
      stop('x must be an n-by-2 or n-by-3 matrix')
    }
  }
  n = dim(x)[1]

  # coordinate conversions
  if (dim(x)[2]==3) {
    # convert cartesian to spherical coordinates
    r = sqrt(x[,1]^2+x[,2]^2+x[,3]^2)
    if (basis=='complex') {
      phi = atan2(x[,2],x[,1])
      theta = acos(x[,3]/r)
    }
    zeros = r<=.Machine$double.eps
  } else {
    # convert spherical to Cartesian coordinates
    theta = x[,1]
    phi = x[,2]
    x = cbind(cos(phi)*sin(theta), sin(phi)*sin(theta), cos(theta))
    zeros = NULL
  }

  # compute spherical harmonics
  if (basis=='real') {

    x = unitvector(x) # normalize vector
    Y = as.vector(.rcosmo.sphericalHarmonics(l,m,x))

  } else if (basis=='complex') {

    if (l==0) { # monopole

      Y = rep(1/2/sqrt(pi),n)

    } else if (l==1) { # dipole

      if (m==-1) {
        Y = 0.5*sqrt(3/2/pi)*sin(theta)*exp(-1i*phi)
      } else if (m==0) {
        Y = 0.5*sqrt(3/pi)*cos(theta)
      } else if (m==1) {
        Y = -0.5*sqrt(3/2/pi)*sin(theta)*exp(1i*phi)
      } else {
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
      }

    } else {

      x = unitvector(x) # normalize vector

      if (m==0) {
        Y = as.vector(.rcosmo.sphericalHarmonics(l,m,x))
      } else {
        Yp = as.vector(.rcosmo.sphericalHarmonics(l,abs(m),x))
        Yn = as.vector(.rcosmo.sphericalHarmonics(l,-abs(m),x))
        if (m>0) {
          Y = (Yp+1i*Yn)/sqrt(2)*(-1)^m # cosine-type harmonics
        } else {
          Y = (Yp-1i*Yn)/sqrt(2) # sine-type harmonics
        }
      }

    }
  } else {
    stop('basis unknown')
  }

  Y[zeros] = 0
  return(Y)

}

.rcosmo.sphericalHarmonics = function (L, m, xyz)
{
  if (L == 0) {
    Y <- matrix(1, dim(xyz)[1], 1)/sqrt(4 * pi)
    return(Y)
  }
  if ((m < -L) || (m > L)) {
    sprintf("Warning: m should be in [-%i,%i]", L)
  }
  else {
    m_abs <- abs(m)
    n <- 1:m_abs
    c_ellm_2 <- prod(1 + m_abs/(L - n + 1))
    c_ellm <- (sqrt(2)/2^m_abs) * sqrt((2 * L + 1)/(4 * pi) *
                                         c_ellm_2)
    x <- xyz[, 1]
    y <- xyz[, 2]
    z <- xyz[, 3]
    phi <- matrix(0, length(x), 1)
    t <- phi
    logic_x3 <- (z == 1 | z == -1)
    phi[logic_x3] <- 0
    logic_x2_1 <- (y >= 0 & z < 1 & z > -1)
    t[logic_x2_1] <- x[logic_x2_1]/sqrt(1 - (z[logic_x2_1])^2)
    t[t > 1] <- 1
    t[t < (-1)] <- -1
    phi[logic_x2_1] = acos(t[logic_x2_1])
    logic_x2_2 <- (y < 0 & z < 1 & z > -1)
    t[logic_x2_2] <- x[logic_x2_2]/sqrt(1 - (z[logic_x2_2])^2)
    t[t > 1] <- 1
    t[t < (-1)] <- -1
    phi[logic_x2_2] <- 2 * pi - acos(t[logic_x2_2])
    if (m > 0) {
      Y <- c_ellm * (1 - z^2)^(m_abs/2) * .rcosmo.jacobiPol(m_abs,
                                                    m_abs, L - m_abs, z) * cos(m_abs * phi)
    }
    else if (m == 0) {
      Y <- sqrt((2 * L + 1)/(4 * pi)) * .rcosmo.jacobiPol(0, 0,
                                                  L, z)
    }
    else {
      Y <- c_ellm * (1 - z^2)^(m_abs/2) * .rcosmo.jacobiPol(m_abs,
                                                    m_abs, L - m_abs, z) * sin(m_abs * phi)
    }
  }
  return(Y)
}

.rcosmo.jacobiPol = function (a, b, L, t)
{
  if (L == 0) {
    YJ <- matrix(1, length(t), 1)
  }
  else if (L == 1) {
    YJ <- (a - b)/2 + (a + b + 2)/2 * t
  }
  else {
    pMisb1 <- matrix(1, length(t), 1)
    pMi <- (a - b)/2 + (a + b + 2)/2 * t
    for (i in seq(2, L, 1)) {
      c <- 2 * i + a + b
      tmppMisb1 <- pMi
      pMi <- ((c - 1) * c * (c - 2) * t * pMi + (c - 1) *
                (a^2 - b^2) * pMi - 2 * (i + a - 1) * (i + b -
                                                         1) * c * pMisb1)/(2 * i * (i + a + b) * (c -
                                                                                                    2))
      pMisb1 <- tmppMisb1
    }
    YJ <- pMi
  }
  return(YJ)
}
