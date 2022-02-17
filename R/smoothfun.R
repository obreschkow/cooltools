#' Smoothed Function
#'
#' @importFrom stats smooth.spline predict runif rnorm
#' @importFrom graphics curve
#'
#' @description Generates a cubic smoothed spline function y=f(x) approximating supplied (x,y)-data with a custom number of degrees of freedom. The routines builds on \code{\link[stats]{smooth.spline}}, but directly returns the smoothed function rather than the fitted model.
#'
#' @param x a vector giving the values of the predictor variable, or a list or a two-column matrix specifying x and y.
#' @param y responses. If y is missing or NULL, the responses are assumed to be specified by x, with x the index vector.
#' @param w optional vector of weights of the same length as x; defaults to all 1.
#' @param df the desired equivalent number of degrees of freedom. Must be in [2,nx], where nx is the number of unique x values. If not given, nx is set to the square root of the number of unique x-values.
#' @param ... additional optional arguments used by \code{\link[stats]{smooth.spline}}.
#'
#' @return Returns a fast and vectorized smoothed function f(x).
#'
#' @author Danail Obreschkow
#'
#' @examples
#' # make random data set
#' set.seed(1)
#' x = runif(100)
#' y = sin(2*pi*x)+rnorm(100, sd=0.5)
#' plot(x,y,pch=16)
#'
#' # smoothed spline
#' f = smoothfun(x, y)
#' curve(f, add=TRUE, col='red')
#'
#' # smoothed spline with custom degree of freedom
#' g = smoothfun(x, y, df=5)
#' curve(g, add=TRUE, col='blue')
#'
#' @seealso \code{\link[stats]{smooth.spline}}
#'
#' @export

smoothfun = function(x, y=NULL, w=NULL, df=NULL, ...) {

  if (is.null(df)) {
    if (is.array(x)) {
      nunique = length(unique(x[,1]))
    } else {
      nunique = length(unique(x))
    }
    df = min(nunique,max(2,round(sqrt(nunique))))
  }

  ss = stats::smooth.spline(x=x, y=y, w=w, df=df, ...)
  return(function(x) stats::predict(ss,x)$y)

}
