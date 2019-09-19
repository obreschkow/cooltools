#' Grid 1D point set
#'
#' @description Generates a 1D grid from a 1D point set, optionally with weights; similar to hist.
#'
#' @param x N-element vector of points
#' @param w optional N-element vector with weights
#' @param n scalar specifying the number of equally space grid cells
#' @param xlim 2-element vector specifying the data range (data cropped if necessary). If not given, xlim is set to the range of x
#'
#' @return Returns a list of items
#' \item{x}{n-element vector of cell-center x-coordinates}
#' \item{xbreak}{(n+1)-element vector of cell-edge x-coordinates}
#' \item{xlim}{range of xbreak, same as input argument xlim, if given}
#' \item{n}{vector of point counts.}
#' \item{m}{vector of weighted point counts (masses); only available if \code{w} is specified.}
#'
#' @author Danail Obreschkow
#'
#' @examples
#' set.seed(1)
#' x = runif(2000)
#' g = griddata(x,xlim=c(0,1))
#' plot(g$x,g$n,xlim=c(0,1),ylim=c(0,max(g$n)),pch=20,type='l')
#'
#' @seealso \code{\link{griddata2}}
#'
#' @export

griddata = function(x, w=NULL, n=20, xlim=NULL) {

  # handle inputs
  if (!is.null(w)) {
    if (!is.vector(w)) stop('If given, w must be a vector.')
    if (length(x)!=length(w)) stop('If given, w must be of the same length as x.')
  }

  # make limits
  if (is.null(xlim)) xlim=range(x)

  # make grid coordinates
  g = list(x = (seq(n)-0.5)/n*(xlim[2]-xlim[1])+xlim[1], # vector of mid-cell x-coordinates
           xbreak = seq(xlim[1], xlim[2], length = n+1), # vector of cell-edge x-coordinates
           xlim = xlim)

  # grid data
  eps = 1e-12
  ix = ceiling(((x-xlim[1])/(xlim[2]-xlim[1])*(1-eps)+eps)*n)
  selection = ix>=1 & ix<=n
  ix = ix[selection]

  # non-weighted counts
  g$n = array(0,n) # number of points in each pixel
  for (i in seq(length(ix))) g$n[ix[i]] = g$n[ix[i]]+1

  # weighted counts
  if (!is.null(w)) {
    g$m = array(0,n) # number of points in each pixel
    w = w[selection]
    for (i in seq(length(ix))) g$m[ix[i]] = g$m[ix[i]]+w[i]
  }

  # return data
  return(g)

}
