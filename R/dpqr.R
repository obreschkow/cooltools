#' d/p/q/r-family for a custom distribution
#'
#' @importFrom stats integrate uniroot
#' @importFrom pracma primes
#'
#' @description Produces the family of d/p/q/r functions associated with a custom distribution function; similarly to the standard families dnorm/pnorm/qnorm/rnorm, dunif/punif/...
#'
#' @param fun distribution function of a single variable; does not have to be normalized
#' @param min,max domain of distribution function; outside this domain \code{fun} will be considered equal to 0. In practice, this should be the most restrictive domain containing (almost) all the mass of \code{fun}.
#'
#' @return Returns a list of items:
#' \item{d(x)}{Probability distribution function (PDF), i.e. a normalised version of \code{fun}, limited to the domain \code{[xmin,xmax]}.}
#' \item{p(x)}{Cumulative distributiont function, defined as the integrated PDF up to x.}
#' \item{q(p)}{Quantile function, returning the position *x*, at which the cumulative probability equals \code{p}.}
#' \item{r(n)}{A vector of \code{n} random numbers drawn from the PDF.}
#'
#' @examples
#'
#' ## Numerically integrate sin(x)
#' f = function(x) sin(x)
#' rsin = dpqr(f,0,pi)$r
#' x = rsin(1e3)
#' hist(x,freq=FALSE)
#' curve(sin(x)/2,0,pi,add=TRUE)
#'
#' @author Danail Obreschkow
#'
#' @export dpqr

dpqr = function(fun,min,max) {
  # fun = function of one variable, represending an arbitrary non-normalised PDF
  # min, max = truncation values of the PDF
  e = (max-min)*1e-12
  norm = integrate(fun,min,max)$value
  d = function(x) fun(x)/norm # PDF
  p = function(x) integrate(d,min,x)$value # CDF
  q = function(p) Vectorize(uniroot(function(x) p(x)-p,c(min-e,max+e))$root) # QF
  r = function(n) q(runif(n)) # RNG
  return(list(d = d, p = Vectorize(p), q = q, r = r))
}
