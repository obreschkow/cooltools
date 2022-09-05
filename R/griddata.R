#' Distribute a point set onto a regular grid
#'
#' @importFrom data.table data.table .N .SD
#'
#' @description Distributes a set of points in D dimensions onto a regular, D-dimensional grid, using a fast nearest neighbor algorithm. Weights can be used optionally.
#'
#' @param x N-element vector of x-coordinates or N-by-D matrix, giving the Cartesian coordinates of N points in D dimensions.
#' @param y optional N-element vector of y-coordinates (only used in D=2 dimensions if x is given as a vector).
#' @param w optional N-element vector with weights.
#' @param n scalar or D-element vector specifying the number of equally space grid cells along each dimension.
#' @param xlim 2-element vector or 2-by-D matrix specifying the rectangular domain on which the grid is produced. If not given, xlim is set to the range of x.
#' @param ylim 2-element vector specifying the range long the y-coordinate (only used in D=2 dimensions if x is given as a vector).
#' @param density logical. If TRUE, the output arrays \code{counts} and \code{mass} are rescaled, such that the total number of points is \code{sum(counts) dV} and the total mass is \code{sum(mass) dV}, where \code{dV} is the volume of the D-dimensional grid cells.
#'
#' @return Returns a list of items
#' \item{counts}{D-dimensional array representing the number of points in each grid cell}
#' \item{mass}{D-dimensional array representing the mass (=sum of weights) of points in each grid cell. Only exists, if argument w is provided.}
#' \item{grid}{List of D elements with the grid properties along each dimension. n: number of grid cells; mid: n-vector of mid-cell coordinates; breaks: (n+1)-vector of cell edges; lim: 2-vector of considered range; delta: cell width.}
#'
#' @author Danail Obreschkow
#'
#' @examples
#'
#' # Distribute 1-dimensional data onto a regular grid
#' npoints = 1e4
#' x = rnorm(npoints)
#' g = griddata(x,xlim=c(-3,3),n=100,density=TRUE)
#' curve(dnorm(x),-3,3)
#' points(g$grid$mid,g$counts/npoints,pch=16)
#'
#' # Distribute 2-dimensional data onto a regular grid
#' x = runif(100,max=2)
#' y = runif(100)
#' g = griddata(x,y,xlim=c(0,2),ylim=c(0,1),n=c(20,10))
#' image(g$grid[[1]]$breaks,g$grid[[2]]$breaks,g$counts,
#'       asp=1,col=grey.colors(100,0,1),xlab='x',ylab='y')
#' points(x,y,col='red',pch=16)
#'
#' # ... same with weights
#' w = runif(100)
#' g = griddata(x,y,w=w,xlim=c(0,2),ylim=c(0,1),n=c(20,10))
#' image(g$grid[[1]]$breaks,g$grid[[2]]$breaks,g$mass,asp=1,col=grey.colors(100,0,1),xlab='x',ylab='y')
#' points(x,y,col='red',pch=16,cex=w)
#'
#' @export

griddata = function(x, y=NULL, w=NULL, n=10, xlim=NULL, ylim=NULL, density=FALSE) {

  # handle inputs
  if (is.list(x)) x = as.matrix(x)
  if (is.data.frame(x)) x = as.matrix(x)
  if (!is.null(y)) {
    if (is.list(y)) y = as.matrix(y)
    if (is.data.frame(x)) y = as.matrix(y)
  }

  # get number of dimensions and rearrange arrays
  if (is.vector(x)) {
    if (is.null(y)) {
      d = 1
      x = cbind(x)
    } else {
      d = 2
      if (length(x)!=length(y)) stop('x and y must be vectors of the same length.')
      x = cbind(x,y)
      rm(y)
    }
  } else {
    if (!is.null(y)) stop('y must not be given if x is not a vector')
    if (length(dim(x))!=2) stop('x must be a vector or a N-by-D matrix.')
    d = dim(x)[2]
  }

  # handle number of cells
  if (length(n)==1) {
    n = rep(n,d)
  } else {
    if (length(n)!=d) stop('n must be a single integer or D-vector of integers')
  }
  n = pmax(1,round(n))

  # handle limits
  limit.given = !is.null(xlim) || !is.null(ylim)
  if (is.null(xlim)) {
    xlim = array(as.vector(apply(x,2,range)),c(2,d))
  } else {
    if (is.vector(xlim)) {
      if (d==1) {
        xlim = cbind(xlim)
      } else if (d==2) {
        if (!is.null(ylim)) xlim = cbind(xlim,xlim)
      } else {
        stop('xlim in wrong format/size.')
      }
    }
  }
  if (dim(xlim)[1]!=2) stop('xlim in wrong format/size.')
  if (dim(xlim)[2]!=d) stop('xlim in wrong format/size.')
  if (!is.null(ylim)) {
    if (d!=2) stop('ylim should only be used with 2-dimensional data.')
    xlim[,2] = ylim
  }

  # handle weights
  if (!is.null(w)) {
    if (length(w)==1) {
      w=NULL
    } else {
      if (!is.vector(w)) stop('If given, w must be a vector.')
      if (length(w)!=dim(x)[1]) stop('If given, w must be a N-vector.')
    }
  }

  # preselect (x,y)-coordinates within the range
  if (limit.given) {
    if (is.null(w)) {
      if (d==1) {
        x = cbind(x[x>=xlim[1,1] & x<=xlim[2,1]])
      } else {
        for (k in seq(d)) {
          x = x[x[,k]>=xlim[1,k] & x[,k]<=xlim[2,k],]
        }
      }
    } else {
      if (d==1) {
        s = which(x>=xlim[1,1] & x<=xlim[2,1])
        x = cbind(x[s])
        w = w[s]
      } else {
        for (k in 1:d) {
          s = which(x[,k]>=xlim[1,k] & x[,k]<=xlim[2,k])
          x = x[s,]
          w = w[s]
        }
      }
    }
  }

  # make grid coordinates
  g = list(grid = list())
  dV = 1
  for (k in 1:d) {
    g$grid[[k]] = list(n = n[k],
                       mid = (seq(n[k])-0.5)/n[k]*(xlim[2,k]-xlim[1,k])+xlim[1,k],
                       breaks = seq(xlim[1,k], xlim[2,k], length = n[k]+1),
                       lim = xlim[,k],
                       delta = (xlim[2,k]-xlim[1,k])/n[k])
    dV = dV*g$grid[[k]]$delta
  }
  if (d==1) g$grid = g$grid[[1]]

  # convert continuous d-dimensional coordinates into discrete 1-dimensional index
  index = rep(1,dim(x)[1])
  f = 1
  for (k in 1:d) {
    i = pmax(1,pmin(n[k],ceiling((x[,k]-xlim[1,k])/(xlim[2,k]-xlim[1,k])*n[k])))
    index = index+(i-1)*f # 1D index
    f = f*n[k]
  }

  # efficiently count number of points of each index
  g$counts = array(tabulate(index,prod(n)),n)
  if (density) g$counts = g$counts/dV

  # efficiently county mass at each index
  if (!is.null(w)) {
    DT = data.table(index=index, w=w)
    q = DT[, c(.N, lapply(.SD, sum)), by=index]
    g$mass = array(0,n)
    g$mass[q$index] = q$w
    if (density) g$mass = g$mass/dV
  }

  # return data
  return(g)
}
