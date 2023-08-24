#' Jackknife Estimation
#'
#' @description Computes the "leave-one-out" Jackknife bias and standard error of an estimator \code{f(x)} of a data-vector \code{x}, or an estimator \code{f(x,y)} of vectors \code{x} and \code{y}. See Efron and Tibshirani (1993) for details
#'
#' @param x data vector
#' @param f estimator function \code{f(x,...)} or \code{f(x,y,...)}
#' @param y optional data vector if \code{f} is a function of two vectors, such as the correlation coefficient \code{cor(x,y)}.
#' @param ... optional arguments to be passed to \code{f}.
#'
#' @return Returns a list with the following components:
#' \item{value}{Default value of the estimator \code{f}.}
#' \item{bias}{Jackknife bias estimate of \code{f}.}
#' \item{unbiased}{Bias-corrected value of \code{f}.}
#' \item{sd}{Jackknife standard error of \code{f}.}
#'
#' @author Danail Obreschkow
#'
#' @export

jackknife = function(x,f,y=NULL,...) {

  # input handling
  x = as.vector(x)
  n = length(x)
  if (!is.null(y)) {
    y = as.vector(y)
    if (length(y)!=n) stop('x and y must be vectors of identical length')
  }

  # jackknife iterations
  q = rep(NA,n)
  if (is.null(y)) {
    q.value = f(x,...)
    for (i in seq(n)) q[i] = f(x[-i],...)
  } else {
    q.value = f(x,y,...)
    for (i in seq(n)) q[i] = f(x[-i],y[-i],...)
  }

  # finalize estimators
  meanq = mean(q)
  q.unbiased = n*q.value-(n-1)*meanq
  q.sd = sqrt(mean((q-meanq)^2)*(n-1))

  # return result
  return(list(value=q.value, bias=q.value-q.unbiased, unbiased=q.unbiased, sd=q.sd))
}
