#' 2D adaptive kernel density estimation
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
#' @param smoothw logical flag; if set TRUE, the smoothing depends on the weighted mass rather than the counts in each pixel.
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
#' @seealso \code{\link{griddata}}
#'
#' @export
#'
kde2 = function(x, y, w=NULL, s=1, n=c(20,20), xlim=range(x), ylim=range(y), sd.min=NULL, sd.max=NULL,
                reflect='', smoothw=FALSE) {

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
  sd.max = max(2*d,sd.max)
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
  g = griddata(x=x,y=y,w=w,n=n+2*h.max,xlim=xlim,ylim=ylim)

  if (is.null(g$mass)) {
    map=g$counts
  } else {
    map=g$mass
  }

  if (smoothw) {
    count = g$mass/sum(g$mass)*sum(g$counts)
  } else {
    count = g$counts
  }
  g$d = kde2stampxx(map, count, h.max, s, sd.min, sd.max, d, n.kernels, unlist(kernel), kern.index, kern.length)[h.max+(1:(n[1]+2*h.max)),h.max+(1:(n[2]+2*h.max))]

  # make boundaries
  if (any(reflect=='left')) g$d[(h.max+1):(2*h.max),] = g$d[(h.max+1):(2*h.max),]+g$d[h.max:1,]
  if (any(reflect=='right')) g$d[(h.max+n[1]):(1+n[1]),] = g$d[(h.max+n[1]):(1+n[1]),]+g$d[(h.max+n[1]+1):(2*h.max+n[1]),]
  if (any(reflect=='bottom')) g$d[,(h.max+1):(2*h.max)] = g$d[,(h.max+1):(2*h.max)]+g$d[,h.max:1]
  if (any(reflect=='top')) g$d[,(h.max+n[2]):(1+n[2])] = g$d[,(h.max+n[2]):(1+n[2])]+g$d[,(h.max+n[2]+1):(2*h.max+n[2])]
  g$d = g$d[h.max+(1:n[1]),h.max+(1:n[2])]

  # crop grid
  g$x = g$grid[[1]]$mid[h.max+(1:n[1])]
  g$y = g$grid[[2]]$mid[h.max+(1:n[2])]
  g$xbreak = g$grid[[1]]$breaks[h.max+(1:(n[1]+1))]
  g$ybreak = g$grid[[2]]$breaks[h.max+(1:(n[2]+1))]
  g$n = g$counts[h.max+(1:n[1]),h.max+(1:n[2])]
  if (!is.null(w)) g$m = g$mass[h.max+(1:n[1]),h.max+(1:n[2])]
  g$xlim = range(g$xbreak)
  g$ylim = range(g$ybreak)

  # remove outputs from griddata
  g$grid = NULL
  g$field = NULL
  g$dV = NULL

  # return result
  return(g)

}
