#' Distribute a point set onto a regular grid
#'
#' @importFrom data.table data.table .N .SD
#'
#' @description Distributes a set of points in D dimensions onto a regular, D-dimensional grid, using a fast nearest neighbor algorithm. Weights can be used optionally.
#'
#' @param x N-element vector (if D=1) or N-by-D matrix (if D>1), giving the Cartesian coordinates of N points in D dimensions.
#' @param w optional N-element vector with weights.
#' @param n scalar or D-element vector specifying the number of equally space grid cells along each dimension.
#' @param min scalar or D-element vector specifying the lower bound of the grid. If not given, min is adjusted to the range of x.
#' @param max scalar or D-element vector specifying the upper bound of the grid. If not given, max is adjusted to the range of x.
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
#' g = griddata(x,min=-3,max=3,n=100,density=TRUE)
#' curve(dnorm(x),-3,3)
#' points(g$grid$mid,g$counts/npoints,pch=16)
#'
#' # Distribute 2-dimensional data onto a regular grid
#' x = runif(100,max=2)
#' y = runif(100)
#' g = griddata(cbind(x,y),min=c(0,0),max=c(2,1),n=c(20,10))
#' image(g$grid[[1]]$breaks,g$grid[[2]]$breaks,g$counts,
#'       asp=1,col=grey.colors(100,0,1),xlab='x',ylab='y')
#' points(x,y,col='red',pch=16)
#'
#' # ... same with weights
#' w = runif(100)
#' g = griddata(cbind(x,y),w,min=c(0,0),max=c(2,1),n=c(20,10))
#' image(g$grid[[1]]$breaks,g$grid[[2]]$breaks,g$mass,asp=1,col=grey.colors(100,0,1),xlab='x',ylab='y')
#' points(x,y,col='red',pch=16,cex=w)
#'
#' @export

griddata = function(x, w=NULL, n=10, min=NULL, max=NULL, density=FALSE) {

  # handle inputs
  if (is.list(x)) x = as.matrix(x)
  if (is.data.frame(x)) x = as.matrix(x)

  # get number of dimensions and rearrange arrays
  if (is.vector(x)) {
    d = 1
    x = cbind(x)
  } else {
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
  limit.given = FALSE
  if (is.null(min)) {
    min = apply(x,2,min)
  } else {
    limit.given = TRUE
    if (length(min)==1) min=rep(min,d)
    if (length(min)!=d) stop('min should be a single number or D-vector.')
  }
  if (is.null(max)) {
    max = apply(x,2,max)
  } else {
    limit.given = TRUE
    if (length(max)==1) max=rep(max,d)
    if (length(max)!=d) stop('max should be a single number or D-vector.')
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

  # preselect coordinates within the range
  if (limit.given) {
    if (is.null(w)) {
      if (d==1) {
        x = cbind(x[x>=min[1] & x<=max[1]])
      } else {
        for (k in seq(d)) {
          x = x[x[,k]>=min[k] & x[,k]<=max[k],]
        }
      }
    } else {
      if (d==1) {
        s = which(x>=min[1] & x<=max[1])
        x = cbind(x[s])
        w = w[s]
      } else {
        for (k in 1:d) {
          s = which(x[,k]>=min[k] & x[,k]<=max[k])
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
                       mid = (seq(n[k])-0.5)/n[k]*(max[k]-min[k])+min[k],
                       breaks = seq(min[k], max[k], length = n[k]+1),
                       min = min[k], max = max[k],
                       delta = (max[k]-min[k])/n[k])
    dV = dV*g$grid[[k]]$delta
  }
  if (d==1) g$grid = g$grid[[1]]

  # convert continuous d-dimensional coordinates into discrete 1-dimensional index
  index = rep(1,dim(x)[1])
  f = 1
  for (k in 1:d) {
    i = pmax(1,pmin(n[k],ceiling((x[,k]-min[k])/(max[k]-min[k])*n[k])))
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
