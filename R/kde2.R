#' 2D kernel density estimation
#'
#' @importFrom pracma meshgrid
#' @importFrom Rcpp sourceCpp
#'
#' @description Produces a 2D kernel density estimation on a 2D grid from a 2D point set, using adaptive smoothing and allowing for the data points to have weights.
#'
#' @param x N-element vector of x-coordinates or N-by-2 matrix of (x,y)-coordinates
#' @param y N-element vector of y-coordinates (only used if x is a vector)
#' @param w optional N-element vector with weights
#' @param s characteristic smoothing length
#' @param n scalar or 2-element vector specifying the number of equally space grid cells
#' @param xlim 2-element vector specifying the x-range
#' @param ylim 2-element vector specifying the y-range
#' @param sd.min optional value, specifying the minimum blurring of any pixel, expressed in standard deviations in units of pixels
#' @param sd.max optional value, specifying the maximum blurring of any pixel, expressed in standard deviations in units of pixels
#' @param reflect vector of characters c('left','right','bottom','top') specifying the edges, where the data should be reflected
#' @param cpp logical flag; if set to TRUE (default) a fast implementation in C++ is used.
#'
#' @return Returns a list of items
#' \item{x}{n-element vector of cell-center x-coordinates.}
#' \item{y}{n-element vector of cell-center y-coordinates.}
#' \item{xbreak}{(n+1)-element vector of cell-edge x-coordinates.}
#' \item{ybreak}{(n+1)-element vector of cell-edge y-coordinates.}
#' \item{n}{2D array of point counts.}
#' \item{m}{2D array of weighted point counts (masses); only available if \code{w} is specified.}
#' \item{d}{2D array of smoothed density field.}
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{griddata2}}
#'
#' @export
#'
kde2 = function(x, y, w=NULL, s=1, n=c(20,20), xlim=range(x), ylim=range(y), sd.min=NULL, sd.max=NULL, reflect='', cpp=TRUE) {

  # handle inputs
  if (length(n)==1) n=c(n,n)
  if (is.matrix(x)) {
    if (dim(x)[2]!=2) stop('x must be a vector or a N-by-2 matrix')
    y = x[,2]
    x = x[,1]
  }

  # make smoothing kernels
  d = 0.1 # step between standard deviations in pixels
  n.sd = 3 # number of standard deviations considered
  n.pix = prod(n)
  if (is.null(sd.min)) {
    sd.min = 0
  }
  if (is.null(sd.max)) {
    sd.max = round(sqrt(n.pix)/4) # maximum standard deviation in pixel
  }
  sd = seq(0,sd.max,by=d) # list of standard deviations
  n.kernels = length(sd)
  kernel = {}
  kernel[[1]] = matrix(c(0,0,0,0,1,0,0,0,0),3,3)
  kern.index = rep(1,n.kernels)
  kern.length = rep(9,n.kernels)
  for (i in seq(2,n.kernels)) {
    kern.index[i] = kern.index[i-1]+kern.length[i-1]
    h = ceiling(sd[i]*n.sd)
    n.side = 2*h+1 # number of pixels per side
    mesh = pracma::meshgrid(seq(-h,h))
    kernel[[i]] = exp(-(mesh$X^2+mesh$Y^2)/2/sd[i]^2)
    kernel[[i]] = kernel[[i]]/sum(kernel[[i]])
    kern.length[i] = n.side^2
  }

  # grid data onto oversized grid
  h.max = (dim(kernel[[n.kernels]])[1]-1)/2
  xlim = c(xlim[1]-(xlim[2]-xlim[1])/n[1]*h.max,xlim[2]+(xlim[2]-xlim[1])/n[1]*h.max)
  ylim = c(ylim[1]-(ylim[2]-ylim[1])/n[2]*h.max,ylim[2]+(ylim[2]-ylim[1])/n[2]*h.max)
  g = griddata2(x,y,w,n+2*h.max,xlim=xlim,ylim=ylim)

  if (is.null(g$m)) {map=g$n} else {map=g$m}

  if (cpp) {

    g$d = kde2stampxx(map, g$n, h.max, s, sd.min, sd.max, d, n.kernels, unlist(kernel), kern.index, kern.length)[h.max+(1:(n[1]+2*h.max)),h.max+(1:(n[2]+2*h.max))]

  } else {

    # this is the old R version
    g$d = array(0,n+4*h.max)
    for (ix in seq(2*h.max+n[1])) {
      for (iy in seq(2*h.max+n[2])) {
        if (g$n[ix,iy]>0) {
          sd.pixel = min(sd.max,max(sd.min,15*s/sqrt(g$n[ix,iy])))
          i = min(n.kernels,round(sd.pixel/d)+1)
          h = (dim(kernel[[i]])[1]-1)/2
          rx = (ix-h+h.max):(ix+h+h.max)
          ry = (iy-h+h.max):(iy+h+h.max)
          g$d[rx,ry] = g$d[rx,ry]+map[ix,iy]*kernel[[i]]
        }
      }
    }
    g$d = g$d[h.max+(1:(n[1]+2*h.max)),h.max+(1:(n[2]+2*h.max))]

  }

  # make boundaries
  if (any(reflect=='left')) g$d[(h.max+1):(2*h.max),] = g$d[(h.max+1):(2*h.max),]+g$d[h.max:1,]
  if (any(reflect=='right')) g$d[(h.max+n[1]):(1+n[1]),] = g$d[(h.max+n[1]):(1+n[1]),]+g$d[(h.max+n[1]+1):(2*h.max+n[1]),]
  if (any(reflect=='bottom')) g$d[,(h.max+1):(2*h.max)] = g$d[,(h.max+1):(2*h.max)]+g$d[,h.max:1]
  if (any(reflect=='top')) g$d[,(h.max+n[2]):(1+n[2])] = g$d[,(h.max+n[2]):(1+n[2])]+g$d[,(h.max+n[2]+1):(2*h.max+n[2])]
  g$d = g$d[h.max+(1:n[1]),h.max+(1:n[2])]

  # crop grid
  g$x = g$x[h.max+(1:n[1])]
  g$y = g$y[h.max+(1:n[2])]
  g$xbreak = g$xbreak[h.max+(1:(n[1]+1))]
  g$ybreak = g$ybreak[h.max+(1:(n[2]+1))]
  g$n = g$n[h.max+(1:n[1]),h.max+(1:n[2])]
  if (!is.null(w)) g$m = g$m[h.max+(1:n[1]),h.max+(1:n[2])]

  # return result
  return(g)

}
