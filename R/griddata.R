#' Grid 1D point set
#'
#' @importFrom data.table data.table .N .SD
#'
#' @description Generates a regular 1D grid from a 1D point set, optionally with weights; similar to hist.
#'
#' @param x N-element vector of points
#' @param w optional N-element vector with weights
#' @param n scalar specifying the number of equally space grid cells
#' @param xlim 2-element vector specifying the data range (data cropped if necessary). If not given, xlim is set to the full range of x.
#'
#' @return Returns a list of items
#' \item{x}{n-element vector of cell-center x-coordinates}
#' \item{xbreak}{(n+1)-element vector of cell-edge x-coordinates}
#' \item{dx}{spacing between x-coordinates}
#' \item{xlim}{range of xbreak, same as input argument xlim, if given}
#' \item{n}{n-element vector giving the number of points in each grid cell}
#' \item{m}{n-element vector of weighted point counts (masses); only available if \code{w} is specified.}
#' \item{d}{normalized number densities corresponding to n, such that sum(d)*dx=1.}
#' \item{c}{normalized mass densities corresponding to m, such that sum(c)*dx=1; only available if \code{w} is specified.}
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
  if (n<1) stop('n must be a positive integer.')
  if (!is.null(w)) {
    if (!is.vector(w)) stop('If given, w must be a vector.')
    if (length(x)!=length(w)) stop('If given, w must be of the same length as x.')
  }

  # make limits
  if (is.null(xlim)) xlim=range(x)

  # make grid coordinates
  g = list(x = (seq(n)-0.5)/n*(xlim[2]-xlim[1])+xlim[1], # vector of mid-cell x-coordinates
           xbreak = seq(xlim[1], xlim[2], length = n+1), # vector of cell-edge x-coordinates
           dx = (xlim[2]-xlim[1])/n, # cell-spacing
           xlim = xlim)

  # preselect x-coordinates within the range
  selection = x>=xlim[1] & x<=xlim[2]
  x = x[selection]
  if (!is.null(w)) w = w[selection]

  # convert continuous x-coordinates to discrete grid indices
  index = pmax(1,pmin(n,ceiling(((x-xlim[1])/(xlim[2]-xlim[1]))*n)))

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

  # density
  g$d = g$n/sum(g$n)/g$dx

  # count mass per cell
  if (!is.null(w)) {
    g$m = rep(0,prod(n))
    g$m[q$index] = q$w
    g$c = g$m/sum(g$m)/g$dx
  }

  # return data
  return(g)

}

