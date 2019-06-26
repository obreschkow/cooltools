#' Mutual information of two random variables
#'
#' @description Computes the mutual information of two random variables X and Y, given their 2D represented in a matrix.
#'
#' @param x either of the following: (1) an m-by-n matrix representing the 2D probability mass function of two random variables X and Y; all elements must be non-negative; the normalization is irrelevant. (2) an n-vector of sampled x-values; in this case y must be specified.
#' @param y optional vector of sampled y-values (only used if \code{x} is a vector of x-values).
#' @param b base of the logarithm in mutual information I(X,Y). Default is e.
#' @param ... additional optional arguments (n, xlim, ylim) for the gridding routine \code{\link{griddata2}}, if x and y vectors are given.
#'
#' @return Returns a list of items:
#' \item{I}{standard mutual information I(X,Y).}
#' \item{N}{normalized mutual information I(X,Y)/sqrt(H(X)*H(Y)), where H is the Shannon information entropy.}
#' \item{pmf}{normalized, gridded probability mass function.}
#'
#' @author Danail Obreschkow
#'
#' @export

mutual = function(x, y=NULL, b=exp(1), ...) {

  if (is.array(x)) {
    if (length(dim(x))!=2) stop('x must be a vector or a 2D array.')
    p = x
  } else {
    if (!is.vector(x)) stop('x must be a vector or a 2D array.')
    if (!is.vector(y)) stop('If x is a vector, y must also be a vector.')
    n = length(x)
    if (length(y)!=n) stop('x and y must have the same number of elements')
    p = griddata2(x,y,...)$n
  }

  p = p/sum(p)

  px = rowSums(p)
  py = colSums(p)

  px.vect = rep(px,length(py))
  py.vect = rep(py,each=length(px))
  p.vect = as.vector(p)

  eps = 1e-300
  I = sum(p.vect*log(p.vect/(px.vect*py.vect+eps)+eps))
  N = I/sqrt(entropy(px)*entropy(py))

  return(list(I=I/log(b), N=N, pmf=p))

}
