#' Generate the d/p/q/r family for an arbitrary distribution
#'
#' @importFrom stats integrate uniroot runif
#'
#' @description
#' Generates the standard \code{d}, \code{p}, \code{q}, and \code{r}
#' functions associated with an arbitrary one-dimensional probability density
#' function, analogous to \code{dnorm}/\code{pnorm}/\code{qnorm}/\code{rnorm}
#' and related families.
#'
#' @param fun A non-negative, vectorised function proportional to a probability density. It does not need to be normalised.
#' @param min,max Lower and upper bounds of the support. The density is assumed to be zero outside this interval.
#'
#' @return Returns a list with the following functions:
#' \item{d(x)}{Probability density function (PDF), i.e. a normalised version of \code{fun} on the domain \code{[min,max]}.}
#' \item{p(x)}{Cumulative distribution function (CDF).}
#' \item{q(p)}{Quantile function.}
#' \item{r(n)}{Generates \code{n} random samples from the distribution.}
#'
#' @examples
#' f = function(x) sin(x)
#' dist = dpqr(f, 0, pi)
#'
#' x = dist$r(1000)
#' hist(x, probability = TRUE)
#' curve(dist$d(x), add = TRUE)
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{rng}}, \code{\link{contourlevel}}
#'
#' @export

dpqr = function(fun, min, max) {

  # Basic checks
  stopifnot(
    is.function(fun),
    length(min) == 1L,
    length(max) == 1L,
    is.finite(min),
    is.finite(max),
    min < max
  )

  # Normalisation
  norm = integrate(fun, lower = min, upper = max)$value

  if (!is.finite(norm) || norm <= 0) {
    stop("The integral of 'fun' over [min, max] must be finite and positive.")
  }

  pdf = function(x) {
    y = numeric(length(x))
    inside = is.finite(x) & x >= min & x <= max
    y[inside] = fun(x[inside]) / norm
    y[is.na(x)] = NA_real_
    y
  }

  cdf_scalar = function(x) {
    if (is.na(x)) return(NA_real_)
    if (x <= min) return(0)
    if (x >= max) return(1)

    integrate(
      fun,
      lower = min,
      upper = x
    )$value / norm
  }

  cdf = Vectorize(cdf_scalar, USE.NAMES = FALSE)

  quantile_scalar = function(prob) {
    if (is.na(prob)) return(NA_real_)

    if (prob < 0 || prob > 1) {
      return(NaN)
    }

    if (prob == 0) return(min)
    if (prob == 1) return(max)

    uniroot(
      function(x) cdf_scalar(x) - prob,
      interval = c(min, max)
    )$root
  }

  quantile = Vectorize(quantile_scalar, USE.NAMES = FALSE)

  random = function(n) {
    quantile(runif(n))
  }

  list(
    d = pdf,
    p = cdf,
    q = quantile,
    r = random
  )
}
