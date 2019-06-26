#' Grid 2D point set
#'
#' @description Generates a 2D grid from a 2D point set, optionally with weights
#'
#' @param x N-element vector of x-coordinates or N-by-2 matrix of (x,y)-coordinates
#' @param y N-element vector of y-coordinates (only used if x is a vector)
#' @param w optional N-element vector with weights
#' @param n scalar or 2-element vector specifying the number of equally space grid cells
#' @param xlim 2-element vector specifying the x-range (data cropped if necessary)
#' @param ylim 2-element vector specifying the y-range (data cropped if necessary)
#'
#' @return Returns a list of items
#' \item{x}{n-element vector of cell-center x-coordinates}
#' \item{y}{n-element vector of cell-center y-coordinates}
#' \item{xbreak}{(n+1)-element vector of cell-edge x-coordinates}
#' \item{ybreak}{(n+1)-element vector of cell-edge y-coordinates}
#' \item{xlim}{range of xbreak, same as input argument xlim, if given}
#' \item{ylim}{range of ybreak, same as input argument ylim, if given}
#' \item{n}{2D array of point counts.}
#' \item{m}{2D array of weighted point counts (masses); only available if \code{w} is specified.}
#'
#' @author Danail Obreschkow
#'
#' @examples
#' set.seed(1)
#' x = runif2(200)
#' counts = griddata2(x,xlim=c(-1,1),ylim=c(-1,1))$n
#' nplot(pty='s',xlim=c(-1,1),ylim=c(-1,1))
#' rasterImage(rasterflip(lim(counts,0,3)/3),-1,-1,1,1,interpolate=FALSE)
#' points(x,col='red',pch=20,cex=0.5)
#'
#' @export

griddata2 = function(x, y=NULL, w=NULL, n=c(20,20), xlim=NULL, ylim=NULL) {

  # handle inputs
  if (length(n)==1) n=c(n,n)
  if (is.matrix(x)) {
    if (dim(x)[2]!=2) stop('x must be a vector or a N-by-2 matrix')
    y = x[,2]
    x = x[,1]
  }
  if (length(x)!=length(y)) stop('x and y must be vectors of the same length')
  if (!is.null(w)) {
    if (!is.vector(w)) stop('If given, w must be a vector.')
    if (length(x)!=length(w)) stop('If given, w must be of the same length as x and y.')
  }

  # make limits
  if (is.null(xlim)) xlim=range(x)
  if (is.null(ylim)) ylim=range(y)

  # make grid coordinates
  g = list(x = (seq(n[1])-0.5)/n[1]*(xlim[2]-xlim[1])+xlim[1], # vector of mid-cell x-coordinates
           y = (seq(n[2])-0.5)/n[2]*(ylim[2]-ylim[1])+ylim[1], # vector of mid-cell y-coordinates
           xbreak = seq(xlim[1], xlim[2], length = n[1]+1), # vector of cell-edge x-coordinates
           ybreak = seq(ylim[1], ylim[2], length = n[2]+1), # vector of cell-edge y-coordinates
           xlim = xlim, ylim = ylim)

  # grid data
  eps = 1e-14
  ix = ceiling((x-xlim[1])/(xlim[2]-xlim[1])*n[1]*(1-eps)+eps)
  iy = ceiling((y-ylim[1])/(ylim[2]-ylim[1])*n[2]*(1-eps)+eps)
  selection = ix>=1 & ix<=n[1] & iy>=1 & iy<=n[2]
  ix = ix[selection]
  iy = iy[selection]

  # non-weighted counts
  g$n = array(0,n) # number of points in each pixel
  for (i in seq(length(ix))) g$n[ix[i],iy[i]] = g$n[ix[i],iy[i]]+1

  # weighted counts
  if (!is.null(w)) {
    g$m = array(0,n) # number of points in each pixel
    w = w[selection]
    for (i in seq(length(ix))) g$m[ix[i],iy[i]] = g$m[ix[i],iy[i]]+w[i]
  }

  # return data
  return(g)

}
