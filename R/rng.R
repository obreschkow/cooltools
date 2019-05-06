#' Random number generator for a custom $d$-dimensional distribution
#'
#' @importFrom stats runif optim optimize
#'
#' @description Brute-force algorithm for generating a series of random number drawn from an $N$-dimensional distribution.
#'
#' @param f function of an $d$-vector represending an $d$-dimensional distribution function. This function must be non-negative on the whole domain.
#' @param n number of random number to be generated
#' @param min,max are $d$-vectors specifying the domain of distribution function; the domain must be finite and should be set as small as possible to increase the speed.
#' @param fmax maximum value of $f$ on its domain. If set to \code{NULL} (default), this value will be determined automatically, using the \code{\link[stats]{optimize}} (if $d=1$) and \code{\link[stats]{optim}} (if $d>1$) function with its default options. A value for \code{fmax} should only be set, if the automatically determined value (see out put list) is incorret.
#'
#' @return Returns list of items:
#' \item{x}{n-by-d matrix of $n$ random $d$-vectors.}
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
#' @export rng

rng = function(f,n,min,max,fmax=NULL) {

  # initialize
  d = length(min)
  if (length(max)!=d) stop('min and max must have the same length.')

  # find maximum of distribution function
  if (is.null(fmax)) {
    if (d==1) {
      fmax = optimize(f,c(min,max),maximum=TRUE)$maximum
    } else {
      xinit = (min+max)/2
      fmax = optim(xinit,f,control=list(fnscale=-1))$value
    }
  }

  # draw random numbers
  x = array(NA,c(n,d))
  k = 1
  ntrials = 0
  while (k<=n) {
    ntrials = ntrials+1
    x[k,] = runif(d,min,max)
    if (runif(1,max=fmax)<f(x[k,])) k=k+1
  }

  # return result
  if (d==1) x=as.vector(x)
  return(list(x=x, fmax=fmax, n=n, ntrials=ntrials))

}
