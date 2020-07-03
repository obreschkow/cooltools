#' Random number generator for a custom d-dimensional distribution
#'
#' @importFrom stats runif optim optimize
#'
#' @description Brute-force algorithm for drawing random numbers from a d-dimensional distribution.
#'
#' @param f function of a d-vector representing a d-dimensional distribution function. This function must be non-negative on the whole domain. It does not need to be normalized. For fast performance, this function should be vectorized, such that it returns an N-element vector if it is given an N-by-D matrix as argument. An automatic warning is produced if the function is not vectorized in this manner.
#' @param n number of random numbers to be generated
#' @param min,max are d-vectors specifying the domain of distribution function; the domain must be finite and should be as restrictive as possible to keep the number of random trials as low as possible.
#' @param fmax maximum value of \code{f} on its domain. If set to \code{NULL} (default), this value will be determined automatically, using the \code{\link[stats]{optimize}} (if d=1) and \code{\link[stats]{optim}} (if d>1) function with its default options. A value for \code{fmax} should be set, if the automatically determined value (see output list) is incorrect.
#' @param seed optional seed for random number generator.
#' @param warn logical flag. If true (default), a warning is produced if the function f is not vectorized.
#'
#' @return Returns list of items:
#' \item{x}{n-by-d matrix of n random d-vectors.}
#' \item{fmax}{maximum value of the distribution function \code{f} on the domain.}
#' \item{n}{number of random vectors (same as argument \code{n}).}
#' \item{ntrials}{number of trials.}
#'
#' @examples
#'
#' ## 1D random number generation (sin-function)
#' f = function(x) sin(x)
#' out = rng(f,1e3,0,pi)
#' hist(out$x,freq=FALSE,xlab='x')
#' curve(sin(x)/2,0,pi,add=TRUE)
#'
#' ## 5D random number generation (5-dimensional sphere)
#' f = function(x) as.numeric(sum(x^2)<=1)
#' out = rng(f,1e4,rep(-1,5),rep(1,5))
#' cat(sprintf('Number of successes over number of trials : %.4f\n',out$n/out$ntrials))
#' cat(sprintf('Expected ratio for n=\u221E : %.4f\n',pi^(5/2)/gamma(1+5/2)/2^5))
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{dpqr}}
#'
#' @export

rng = function(f,n,min,max,fmax=NULL,seed=NULL,warn=TRUE) {

  # initialize
  d = length(min)
  if (length(max)!=d) stop('min and max must have the same length.')
  if (!is.null(seed)) set.seed(seed)

  # check if f is vectorized
  if (length(f(rbind(min,min)))==1 & length(f(rbind(min,min,min)))==1) {
    if (warn) cat('WARNING: use rng with vectorized function for faster performance.\n')
    if (d==1) {
      fvect = Vectorize(f)
    } else {
      fvect = function(x) apply(x,1,f)
    }
  } else if (length(f(rbind(min,min)))==2 & length(f(rbind(min,min,min)))==3) {
    fvect = f
  } else {
    stop('function f is invalid')
  }

  # find maximum of distribution function
  if (is.null(fmax)) {
    if (d==1) {
      fmax = optimize(fvect,c(min,max),maximum=TRUE)$objective
    } else {
      xinit = (min+max)/2
      fmax = optim(xinit,function(x) fvect(matrix(x,nrow=1)),control=list(fnscale=-1))$value
    }
  }

  # draw random numbers
  x = array(NA,c(n,d))
  ntrials = 0
  k = 0
  while (k<n) {
    dn = n-k
    xtrial = array(runif(d*dn,min,max),c(dn,d))
    ntrials = ntrials+dn
    feval = fvect(xtrial)
    s = feval>runif(dn,max=fmax)
    l = sum(s)
    if (l>0) {
      x[(k+1):(k+l),] = xtrial[s,]
      k = k+l
    }
  }

  # return result
  if (d==1) x=as.vector(x)
  return(list(x=x, fmax=fmax, n=n, ntrials=ntrials))

}
