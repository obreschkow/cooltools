#' Monte Carlo and Quasi-Monte Carlo integration in any dimension
#'
#' @importFrom stats runif sd
#' @importFrom pracma primes
#'
#' @description Numerical integration using a Monte Carlo (MC) or Quasi-Monte Carlo (QMC) algorithm. These algorithms are of low order (1/sqrt(n) for MC, log(n)/n for QMC in one dimension) compared to the typical orders of 1D deterministic integrators, such as those available in the \code{integrate} function. The MC and QMC integrators are suitable to compute D-dimensional integrals with D>>1, since the order of most deterministic methods deteriorates exponentially with D, whereas the order of MC remains 1/sqrt(n), irrespective of D, and the order of QMC only deteriorates slowly with D as log(n)^D/n.
#'
#' @param f scalar function of a D-vector to be integrated numerically; for fast performance, this function should be vectorized, such that it returns an N-element vector if it is given an N-by-D matrix as argument. An automatic warning is produced if the function is not vectorized in this manner.
#' @param a D-vector with lower limit(s) of the integration
#' @param b D-vector with upper limit(s) of the integration
#' @param n approximate number of random evaluations. (The exact number is max(1,round(sqrt(n)))^2.)
#' @param qmc logical flag. If false (default), pseudo-random numbers are used; if true, quasi-random numbers, generated from an additive recurrence algorithm are used.
#' @param seed optional seed for random number generator. Only used if \code{qmc} is false.
#' @param warn logical flag. If true (default), a warning is produced if the function f is not vectorized.
#'
#' @return Returns a list of items:
#' \item{value}{the best estimate of the integral.}
#' \item{error}{an estimate of the statistical 1-sigma uncertainty.}
#' \item{iterations}{exact number of evaluations (close to n).}
#'
#' @examples
#'
#' ## Numerically integrate sin(x)
#' f = function(x) sin(x)
#' m = mcintegral(f,0,pi)
#' cat(sprintf('Integral = %.3f\u00B1%.3f (true value = 2)\n',m$value,m$sigma))
#'
#' ## Numerically compute the volume of a unit sphere
#' sphere = function(x) as.numeric(rowSums(x^2)<=1) # this is vectorized
#' vmc = mcintegral(sphere,rep(-1,3),rep(1,3),seed=1)
#' vqmc = mcintegral(sphere,rep(-1,3),rep(1,3),qmc=TRUE)
#' cat(sprintf('Volume of unit sphere = %.3f\u00B1%.3f (MC)\n',vmc$value,vmc$error))
#' cat(sprintf('Volume of unit sphere = %.3f\u00B1%.3f (QMC)\n',vqmc$value,vqmc$error))
#' cat(sprintf('Volume of unit sphere = %.3f (exact)\n',4*pi/3))
#'
#' @author Danail Obreschkow
#'
#' @export

mcintegral = function(f,a,b,n=1e5,qmc=FALSE,seed=NULL,warn=TRUE) {

  # basic checks
  if (n<10) stop('n must be >= 10.')
  if (length(a)!=length(b)) stop('a and b must be vectors of the same length.')

  # check if f is vectorized
  if (length(f(rbind(a,a)))==1 & length(f(rbind(a,a,a)))==1) {
    vectorized = FALSE
    if (warn) cat('WARNING: use mcintegral with vectorized function for faster performance.\n')
  } else if (length(f(rbind(a,a)))==2 & length(f(rbind(a,a,a)))==3) {
    vectorized = TRUE
  } else {
    stop('function f is invalid')
  }

  # initialize
  d = length(a) # number of dimensions
  m = max(1,round(sqrt(n))) # number of iterations and evaluations per iteration
  dx = prod(b-a)/m
  Q = array(0,m)
  if (qmc) {
    np = d*log(d*sqrt(log(d+2)))*1.02+20 # upper bound for n, such that the nb of primes<=n is >=d
    alpha = sqrt(pracma::primes(np)) # vector of irrational numbers
  } else {
    if (!is.null(seed)) set.seed(seed)
  }

  for (i in seq(m)) {

    # generate random numbers
    if (qmc) {
      x = array(NA,c(m,d))
      for (j in seq(d)) x[,j] = (seq((i-1)*m+1,i*m)*(1+alpha[j]))%%1*(b[j]-a[j])+a[j]
    } else {
      x = t(array(runif(m*d),c(d,m))*(b-a)+a)
    }

    # evaluate integral
    if (vectorized) {
      Q[i] = sum(f(x))*dx
    } else {
      for (j in seq(m)) Q[i] = Q[i]+f(x[j,])
      Q[i] = Q[i]*dx
    }

  }

  # return result
  return(list(value = mean(Q), error = sd(Q)/sqrt(m), iterations=m^2))

}
