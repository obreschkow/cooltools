#' Linear Function Fitter
#'
#' Fits a global linear model \eqn{y = a + b x} and returns a function
#' that predicts \eqn{y} for arbitrary \eqn{x}, similar in spirit to
#' \code{\link[stats]{approxfun}}, but using a single least-squares line
#' instead of piecewise interpolation.
#'
#' @param x Numeric vector of predictor values.
#' @param y Numeric vector of response values.
#' @param na.rm Logical; if \code{TRUE}, remove \code{NA}, \code{NaN}, and
#'   infinite values before fitting (default: \code{FALSE}).
#' @param ... Additional arguments passed to \code{\link[stats]{lm}}.
#'
#' @return A function \code{f(xnew)} that evaluates the fitted linear
#' regression at numeric values \code{xnew}.
#'
#' @details
#' This is a convenience wrapper around \code{\link[stats]{lm}}.
#' It returns a callable function analogous to \code{approxfun}, but with
#' a single global linear fit:
#' \deqn{f(x) = a + b x,}
#' where \eqn{a} and \eqn{b} are the intercept and slope from a
#' least-squares regression of \code{y} on \code{x}.
#'
#' @examples
#' set.seed(1)
#' x = 1:10
#' y = 2 + 3 * x + rnorm(10)
#' f = linfun(x, y)
#' plot(x,y)
#' curve(f, col='red', add = TRUE) # show linear fit
#' points(6.6,f(6.6),col='red') # show predicted y-value at x = 6.6
#'
#' @seealso [stats::lm()], [stats::approxfun()]
#' @export
linfun <- function(x, y, na.rm = FALSE, ...) {
  if (na.rm) {
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
  }
  fit <- lm(y ~ x, ...)
  function(xnew) as.numeric(predict(fit, newdata = data.frame(x = xnew)))
}
