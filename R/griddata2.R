#' Grid 2D point set
#'
#' @importFrom data.table data.table .N .SD
#'
#' @description Generates a 2D grid from a 2D point set, optionally with weights
#'
#' @param x N-element vector of x-coordinates or N-by-2 matrix of (x,y)-coordinates
#' @param y N-element vector of y-coordinates (only used if x is a vector)
#' @param w optional N-element vector with weights
#' @param n scalar or 2-element vector specifying the number of equally space grid cells along each direction.
#' @param xlim 2-element vector specifying the x-range (data cropped if necessary). If not given, xlim is set to the range of x.
#' @param ylim 2-element vector specifying the y-range (data cropped if necessary). If not given, ylim is set to the range of y.
#'
#' @return Returns a list of items
#' \item{x}{n-element vector of cell-center x-coordinates}
#' \item{y}{n-element vector of cell-center y-coordinates}
#' \item{xbreak}{(n+1)-element vector of cell-edge x-coordinates}
#' \item{ybreak}{(n+1)-element vector of cell-edge y-coordinates}
#' \item{dx}{spacing between x-coordinates}
#' \item{dy}{spacing between y-coordinates}
#' \item{xlim}{range of xbreak, same as input argument xlim, if given}
#' \item{ylim}{range of ybreak, same as input argument ylim, if given}
#' \item{n}{2D array of point counts.}
#' \item{d}{normalized number densities corresponding to n, such that \code{sum(d)*dx*dy=1}.}
#' \item{m}{2D array of weighted point counts (masses); only available if \code{w} is specified.}
#' \item{c}{2D array of normalized mass densities corresponding to m, such that sum(c)*dx*dy=1; only available if \code{w} is specified.}
#'
#' @author Danail Obreschkow
#'
#' @examples
#' x = runif(100,min=0,max=2)
#' y = runif(100,min=0,max=1)
#' g = griddata2(x,y,xlim=c(0,2),ylim=c(0,1),n=c(20,10))
#' image(g$xbreak,g$ybreak,g$n,asp=1,col=grey.colors(100,0,1))
#' points(x,y,col='red',pch=20,cex=0.5)
#'
#' @seealso \code{\link{griddata}}
#'
#' @export

griddata2 = function(x, y=NULL, w=NULL, n=c(20,20), xlim=NULL, ylim=NULL) {

  # handle inputs
  if (length(n)==1) n=c(n,n)
  if (is.list(x)) x = as.matrix(x)
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
  g$dx = g$xbreak[2]-g$xbreak[1]
  g$dy = g$ybreak[2]-g$ybreak[1]

  # preselect (x,y)-coordinates within the range
  s = x>=xlim[1] & x<=xlim[2] & y>=ylim[1] & y<=ylim[2]
  x = x[s]
  y = y[s]
  w = w[s]

  # convert continuous (x,y)-coordinates to discrete grid indices
  ix = pmax(1,pmin(n[1],ceiling(((x-xlim[1])/(xlim[2]-xlim[1]))*n[1])))
  iy = pmax(1,pmin(n[2],ceiling(((y-ylim[1])/(ylim[2]-ylim[1]))*n[2])))
  index = (iy-1)*n[1]+ix # 1D index

  # efficiently count number of duplicates and, optionally, total mass for cell index
  if (is.null(w)) {
    DT = data.table(index=index)
    q = DT[,.N,by=index]
  } else {
    DT = data.table(index=index, w=w)
    q = DT[, c(.N, lapply(.SD, sum)), by=index]
  }

  # count points per cell
  g$n = rep(0,prod(n))
  g$n[q$index] = q$N
  g$n = array(g$n,n)

  # density
  g$d = g$n/sum(g$n)/g$dx/g$dy

  # count mass per cell
  if (!is.null(w)) {
    g$m = rep(0,prod(n))
    g$m[q$index] = q$w
    g$m = array(g$m,n)
    g$c = g$m/sum(g$m)/g$dx/g$dy
  }

  # return data
  return(g)
}
