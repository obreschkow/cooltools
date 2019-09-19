#' Mutual information of two random variables
#'
#' @description Computes the mutual information of two random variables X and Y, given their 2D represented in a matrix.
#'
#' @param x either of the following: (1) an m-by-n matrix representing the 2D probability mass function of two random variables X and Y; all elements must be non-negative; the normalization is irrelevant. (2) an n-vector of sampled x-values; in this case y must be specified.
#' @param y optional vector of sampled y-values (only used if \code{x} is a vector of x-values).
#' @param b base of the logarithm in mutual information I(X,Y). Default is e.
#' @param n scalar or 2-element vector specifying the number of equally space grid cells. Only used if x and y are vectors. If not provided, the default is n=0.2*sqrt(length(x)), bound between 2 and 1000. Note that n~sqrt(length(x)) keeps the mutual information constant for random data sets of different size.
#' @param xlim 2-element vector specifying the x-range (data cropped if necessary). Only used if x and y are vectors. If not given, xlim is set to the range of x.
#' @param ylim 2-element vector specifying the y-range (data cropped if necessary). Only used if x and y are vectors. If not given, ylim is set to the range of y.
#'
#' @return Returns a list of items:
#' \item{I}{standard mutual information I(X,Y).}
#' \item{N}{normalized mutual information I(X,Y)/sqrt(H(X)*H(Y)), where H is the Shannon information entropy.}
#'
#' @author Danail Obreschkow
#'
#' @export

mutual = function(x, y=NULL, b=exp(1), n=NULL, xlim=NULL, ylim=NULL) {

  eps = 1e-16 # to avoid divisions by zero (do not choose a smaller number, to avoid diverging nromalized entropy in some cases)

  if (is.array(x)) {
    if (length(dim(x))!=2) stop('x must be a vector or a 2D array.')
    if (!is.null(n)) stop('n can only be specified if x and y are vectors.')
    if (!is.null(xlim)) stop('xlim can only be specified if x and y are vectors.')
    if (!is.null(ylim)) stop('ylim can only be specified if x and y are vectors.')
    p = x
  } else {
    if (!is.vector(x)) stop('x must be a vector or a 2D array.')
    if (!is.vector(y)) stop('If x is a vector, y must also be a vector.')
    if (length(x)!=length(y)) stop('x and y must have the same number of elements')
    if (is.null(n)) n=min(1000,max(2,round(0.2*sqrt(length(x)))))
    if (is.null(xlim)) xlim=range(x)+c(0,eps)
    if (is.null(ylim)) ylim=range(y)+c(0,eps)
    p = griddata2(x,y,n=n,xlim=xlim,ylim=ylim)$n
  }

  p = p/sum(p)

  px = rowSums(p)
  py = colSums(p)

  px.vect = rep(px,length(py))
  py.vect = rep(py,each=length(px))
  p.vect = as.vector(p)

  I = sum(p.vect*log(p.vect/(px.vect*py.vect+eps)+eps))
  N = I/sqrt(entropy(px)*entropy(py)+eps)

  return(list(I=I/log(b), N=N))

}
