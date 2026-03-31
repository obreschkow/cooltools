#' Distribute a point set onto a regular grid
#'
#' @importFrom data.table data.table
#'
#' @description Distributes a set of points in D dimensions onto a regular, D-dimensional grid, using a fast nearest-neighbor algorithm. Weights can be used optionally.
#'
#' @param x N-element vector (if D=1) or N-by-D matrix (if D>1), giving the Cartesian coordinates of N points in D dimensions.
#' @param w optional N-element vector with weights.
#' @param n scalar or D-element vector specifying the number of equally spaced grid cells along each dimension.
#' @param min optional scalar or D-element vector specifying the lower bound of the grid. If not given, \code{min} is adjusted to the range of \code{x}.
#' @param max optional scalar or D-element vector specifying the upper bound of the grid. If not given, \code{max} is adjusted to the range of \code{x}.
#' @param type character string specifying the normalization of the output. Must be one of:
#' \itemize{
#'   \item \code{"counts"}: returns the number of points in each cell, or the sum of weights if \code{w} is given. Thus the total number of points (or total mass, if weights are given) is \code{sum(field)}.
#'   \item \code{"density"}: returns the density, such that the total number of points (or total mass, if weights are given) is \code{sum(field) * dV}.
#'   \item \code{"probability"}: returns a probability density, such that \code{sum(field) * dV = 1}.
#'   \item \code{"mean"}: returns the mean weight in each cell. This option requires \code{w} to be given. Cells with no data are assigned \code{NA}.
#' }
#'
#' @return Returns a list of items
#' \item{field}{D-dimensional array representing the value in each grid cell. See parameter \code{type} for more details.}
#' \item{grid}{List of D elements with the grid properties along each dimension: \code{n} = number of grid cells; \code{mid} = n-vector of mid-cell coordinates; \code{breaks} = (n+1)-vector of cell edges; \code{min}, \code{max} = considered range; \code{delta} = cell width.}
#' \item{dV}{Single number representing the volume of the D-dimensional grid cells.}
#'
#' @author Danail Obreschkow
#'
#' @examples
#'
#' # Distribute 1-dimensional data onto a regular grid
#' npoints = 1e4
#' x = rnorm(npoints)
#' g = griddata(x,min=-3,max=3,n=100,type='probability')
#' curve(dnorm(x),-3,3)
#' points(g$grid$mid,g$field,pch=16)
#'
#' # Distribute 2-dimensional data onto a regular grid
#' x = runif(100,max=2)
#' y = runif(100)
#' g = griddata(cbind(x,y),min=c(0,0),max=c(2,1),n=c(20,10))
#' image(g$grid[[1]]$breaks,g$grid[[2]]$breaks,g$field,
#'       asp=1,col=grey.colors(100,0,1),xlab='x',ylab='y')
#' points(x,y,col='red',pch=16)
#'
#' # ... same with weights
#' w = runif(100)
#' g = griddata(cbind(x,y),w,min=c(0,0),max=c(2,1),n=c(20,10))
#' image(g$grid[[1]]$breaks,g$grid[[2]]$breaks,g$field,
#'       asp=1,col=grey.colors(100,0,1),xlab='x',ylab='y')
#' points(x,y,col='red',pch=16,cex=w)
#'
#' # ... mean weight in each cell
#' g = griddata(cbind(x,y),w,min=c(0,0),max=c(2,1),n=c(20,10),type='mean')
#' image(g$grid[[1]]$breaks,g$grid[[2]]$breaks,g$field,
#'       asp=1,col=grey.colors(100,0,1),xlab='x',ylab='y')
#' points(x,y,col='red',pch=16,cex=w)
#'
#' @export

griddata = function(x, w=NULL, n=10, min=NULL, max=NULL,
                    type=c('counts','density','probability','mean')) {

  type = match.arg(type)

  # handle inputs
  if (is.list(x)) x = as.matrix(x)
  if (is.data.frame(x)) x = as.matrix(x)

  # get number of dimensions and rearrange arrays
  if (is.vector(x)) {
    d = 1
    x = cbind(x)
  } else {
    if (length(dim(x)) != 2) stop('x must be a vector or a N-by-D matrix.')
    d = dim(x)[2]
  }
  npoints = nrow(x)

  # handle number of cells
  if (length(n) == 1) {
    n = rep(n, d)
  } else {
    if (length(n) != d) stop('n must be a single integer or D-vector of integers.')
  }
  n = pmax(1, round(n))

  # handle limits
  limit.given = FALSE
  if (is.null(min)) {
    if (npoints == 1) {
      min = as.vector(x - 1)
    } else {
      min = apply(x, 2, min)
    }
  } else {
    limit.given = TRUE
    if (length(min) == 1) min = rep(min, d)
    if (length(min) != d) stop('min must be a single number or D-vector.')
  }

  if (is.null(max)) {
    if (npoints == 1) {
      max = as.vector(x + 1)
    } else {
      max = apply(x, 2, max)
    }
  } else {
    limit.given = TRUE
    if (length(max) == 1) max = rep(max, d)
    if (length(max) != d) stop('max must be a single number or D-vector.')
  }

  if (any(max <= min)) stop('Each element of max must be greater than min.')

  # handle weights
  if (!is.null(w)) {
    if (length(w) == 1) {
      w = rep(w, npoints)
    } else {
      if (is.array(w)) w = as.vector(w)
      if (!is.vector(w)) stop('If given, w must be a vector.')
      if (length(w) != npoints) stop('If given, w must be a N-vector.')
    }
  }

  if (type == 'mean' && is.null(w)) {
    stop("type='mean' requires weights w.")
  }

  # preselect coordinates within the range
  if (limit.given) {
    keep = rep(TRUE, nrow(x))
    for (k in 1:d) {
      keep = keep & x[,k] >= min[k] & x[,k] <= max[k]
    }
    x = x[keep, , drop = FALSE]
    if (!is.null(w)) w = w[keep]
  }
  npoints = nrow(x)

  # make grid coordinates
  g = list(grid = list(), dV = 1)
  for (k in 1:d) {
    g$grid[[k]] = list(
      n = n[k],
      mid = (seq(n[k]) - 0.5) / n[k] * (max[k] - min[k]) + min[k],
      breaks = seq(min[k], max[k], length.out = n[k] + 1),
      min = min[k],
      max = max[k],
      delta = (max[k] - min[k]) / n[k]
    )
    g$dV = g$dV * g$grid[[k]]$delta
  }
  if (d == 1) g$grid = g$grid[[1]]

  # convert continuous d-dimensional coordinates into discrete 1-dimensional index
  index = rep(1, npoints)
  f = 1
  for (k in 1:d) {
    i = pmax(1, pmin(n[k], ceiling((x[,k] - min[k]) / (max[k] - min[k]) * n[k])))
    index = index + (i - 1) * f
    f = f * n[k]
  }

  if (is.null(w)) {
    # efficiently count number of points of each index
    g$field = array(tabulate(index, prod(n)), n)
  } else {
    DT = data.table(index = index, w = w)

    if (type == 'mean') {
      # mean weight in each cell; undefined cells remain NA
      q = DT[, list(w = mean(w)), by = index]
      g$field = array(NA_real_, n)
      g$field[q$index] = q$w
    } else {
      # efficiently count weighted sum at each index
      q = DT[, list(w = sum(w)), by = index]
      g$field = array(0, n)
      g$field[q$index] = q$w
    }
  }

  # apply custom normalization
  if (type == 'density') {
    g$field = g$field / g$dV
  } else if (type == 'probability') {
    s = sum(g$field * g$dV)
    if (s == 0) stop('Cannot normalize to probability: no points remain in the grid.')
    g$field = g$field / s
  }

  return(g)
}
