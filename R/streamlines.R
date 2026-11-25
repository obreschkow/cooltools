#' Draw streamlines of a two-dimensional vector field
#'
#' Draws streamlines of a planar vector field \eqn{f(x,y)} by integrating
#' forward and backward from a set of starting points. Starting points can be
#' provided as a single integer, a length-2 integer vector, or an \eqn{n \times 2}
#' matrix of coordinates.
#'
#' @param f A vectorized function of the form `f(x,y)` returning an
#'   \eqn{n \times 2} matrix of vector components. Each row must depend only
#'   on the corresponding input coordinates. Non-finite or zero vectors
#'   terminate integration in the corresponding direction.
#' @param xlim Numeric vector of length 2 giving the x-limits of the plot.
#' @param ylim Numeric vector of length 2 giving the y-limits of the plot.
#' @param points Starting points for the streamlines. This can be:
#'   \itemize{
#'     \item a single integer, interpreted as the number of quasi-random starting points;
#'     \item a numeric vector of length 2, interpreted as the numbers of starting
#'       points in x- and y-direction;
#'     \item a numeric matrix with 2 columns, giving explicit starting coordinates.
#'   }
#' @param nsteps Integer giving the number of stored integration points in each
#'   direction, including the starting point.
#' @param arrows Controls where arrows are drawn. This can be:
#'   \itemize{
#'     \item `FALSE`: no arrows are drawn;
#'     \item `TRUE`: one arrow is drawn on every streamline, centered on its
#'       starting point and aligned with the local field direction;
#'     \item an integer vector: arrows are drawn only on the corresponding
#'       streamlines, using the indices of the starting points;
#'     \item a numeric matrix with 2 columns: arrows are drawn at the specified
#'       coordinates, aligned with the local field direction at those points.
#'   }
#' @param arrow.size Length of the arrow shaft as a fraction of
#'   the plot diagonal, independent of the integration step size.
#' @param add Logical; if `FALSE` a new plot is created, otherwise streamlines are
#'   added to the existing plot.
#' @param nplot Maximum number of plotted vertices per integration direction
#'   (including the starting and final valid points). Use `Inf` to plot all
#'   integration points. Does not affect integration or returned coordinates.
#' @param ... Further graphical parameters passed to [graphics::lines()] and
#'   [graphics::arrows()]. Graphical parameters are shared by all streamlines.
#'
#' @details
#' The integration uses a fixed Euler step length based on the plot diagonal,
#' \deqn{d = \sqrt{dx^2 + dy^2} / nsteps,}
#' where `dx = diff(range(xlim))` and `dy = diff(range(ylim))`.
#' Only the direction of the vector field is used, via [unitvector()], so the
#' result shows streamlines rather than trajectories with speed information.
#' Forward and backward integration are vectorized together, and terminated
#' paths are excluded from subsequent field evaluations.
#'
#' Plotting vertices are uniformly subsampled along each valid path when
#' necessary, preserving its starting and final valid points. All streamlines
#' are drawn in a single graphics call, separated by missing coordinates.
#'
#' If `add = FALSE`, a new plot is initialized using [plot()].
#' Arrows are centered on the starting points or on explicitly specified
#' coordinates and aligned with the local field direction.
#'
#' @return
#' Invisibly returns a list with components:
#' \itemize{
#'   \item `forward`: array of forward-integrated coordinates;
#'   \item `backward`: array of backward-integrated coordinates;
#'   \item `start`: matrix of starting coordinates.
#' }
#' The arrays have dimensions `c(number of starting points, 2, nsteps)`.
#' Unused entries after termination are `NA`.
#'
#' @examples
#' f = function(x,y) {
#'   vx = -x-y
#'   vy = -y+(x-0.5)
#'   cbind(vx,vy)
#' }
#'
#' streamlines(f, c(-1,1), c(-1,1), 50, 500, col = "#5555ff")
#' streamlines(f, c(-1,1), c(-1,1), 20, 200, arrows = TRUE, add = FALSE,
#'             col = "#5555ff")
#'
#' @export
streamlines = function(f, xlim, ylim, points = 20, nsteps = 100,
                       arrows = FALSE, arrow.size=0.004, add = FALSE,
                       nplot = 200, ...) {

  if (!is.function(f)) stop('"f" must be a function.')
  if (!is.numeric(xlim) || length(xlim)!=2 || any(!is.finite(xlim)) || diff(xlim)<=0)
    stop('"xlim" must contain two increasing finite coordinates.')
  if (!is.numeric(ylim) || length(ylim)!=2 || any(!is.finite(ylim)) || diff(ylim)<=0)
    stop('"ylim" must contain two increasing finite coordinates.')
  if (!is.numeric(nsteps) || length(nsteps)!=1 || !is.finite(nsteps) ||
      nsteps<2 || nsteps!=floor(nsteps) || nsteps>.Machine$integer.max)
    stop('"nsteps" must be a single integer >= 2.')
  if (!is.numeric(nplot) || length(nplot)!=1 || is.na(nplot) ||
      nplot<2 || nplot!=floor(nplot))
    stop('"nplot" must be an integer >= 2 or Inf.')
  if (!is.numeric(arrow.size) || length(arrow.size)!=1 ||
      !is.finite(arrow.size) || arrow.size<0)
    stop('"arrow.size" must be a finite non-negative scalar.')
  nsteps = as.integer(nsteps)

  # Determine starting points
  if (is.numeric(points) && is.null(dim(points)) && length(points)%in%c(1,2)) {
    if (any(!is.finite(points) | points<1 | points!=floor(points)))
      stop('Starting point counts must be positive integers.')
    if (length(points)==1) {
      r = rng(function(x) rep(1,nrow(x)),points,
              min=c(xlim[1],ylim[1]),max=c(xlim[2],ylim[2]),quasi=TRUE)$x
      px = r[,1]
      py = r[,2]
    } else {
      rx = midseq(xlim[1],xlim[2],points[1])
      ry = midseq(ylim[1],ylim[2],points[2])
      grid = expand.grid(rx,ry)
      px = grid[[1]]
      py = grid[[2]]
    }
  } else if (is.numeric(points) && is.matrix(points) && ncol(points)==2) {
    px = points[,1]
    py = points[,2]
  } else {
    stop('unknown format of input argument "points"')
  }

  np = length(px)
  diagonal = sqrt(diff(xlim)^2+diff(ylim)^2)
  d = diagonal/nsteps

  # Normalize valid field vectors only
  velocity = function(x,y) {
    v = f(x,y)
    if (!is.matrix(v) || !is.numeric(v) || nrow(v)!=length(x) || ncol(v)!=2)
      stop('"f" must return a numeric matrix with one row per point and two columns.')
    good = which(is.finite(v[,1]) & is.finite(v[,2]) & (v[,1]!=0 | v[,2]!=0))
    out = matrix(NA_real_,length(x),2)
    if (length(good)) out[good,] = unitvector(v[good,,drop=FALSE])
    out
  }

  # Integrate both directions simultaneously, keeping only active paths
  p = q = array(NA_real_,c(np,2,nsteps))
  p[,1,1] = q[,1,1] = px
  p[,2,1] = q[,2,1] = py
  pos = cbind(rep(px,2),rep(py,2))
  direction = rep(c(1,-1),each=np)
  active = which(is.finite(pos[,1]) & is.finite(pos[,2]))
  last = rep(1L,2*np)

  for (i in seq_len(nsteps-1L)) {
    if (!length(active)) break
    v = velocity(pos[active,1],pos[active,2])
    good = which(is.finite(v[,1]) & is.finite(v[,2]))
    active = active[good]
    if (!length(active)) break

    nextpos = pos[active,,drop=FALSE] +
      v[good,,drop=FALSE]*(direction[active]*d)
    good = which(is.finite(nextpos[,1]) & is.finite(nextpos[,2]))
    active = active[good]
    if (!length(active)) break
    pos[active,] = nextpos[good,,drop=FALSE]
    last[active] = i+1L

    # Store directly in the two return arrays, avoiding a third full array
    forward = active[active<=np]
    backward = active[active>np]
    if (length(forward)) p[forward,,i+1L] = pos[forward,,drop=FALSE]
    if (length(backward)) q[backward-np,,i+1L] = pos[backward,,drop=FALSE]
  }

  # Draw plot if requested
  if (!add) plot(NULL,xlab='',ylab='',xlim=xlim,ylim=ylim)

  # Subsample each valid path, preserving both endpoints
  plotindex = function(n) {
    if (n<=nplot) seq_len(n) else unique(round(seq(1,n,length.out=nplot)))
  }
  coords = vector('list',np)
  for (i in seq_len(np)) {
    kp = plotindex(last[i])
    kq = rev(plotindex(last[np+i]))
    coords[[i]] = cbind(c(q[i,1,kq],p[i,1,kp[-1L]],NA_real_),
                        c(q[i,2,kq],p[i,2,kp[-1L]],NA_real_))
  }
  if (np) {
    xy = do.call(rbind,coords)
    graphics::lines(xy[,1],xy[,2],...)
  }

  # Draw all arrows in one graphics call
  if (!identical(arrows,FALSE)) {
    if (identical(arrows,TRUE)) {
      ax = px
      ay = py
    } else if (is.numeric(arrows) && is.null(dim(arrows))) {
      ii = unique(as.integer(arrows))
      ii = ii[is.finite(ii) & ii>=1 & ii<=np]
      ax = px[ii]
      ay = py[ii]
    } else if (is.numeric(arrows) && is.matrix(arrows) && ncol(arrows)==2) {
      ax = arrows[,1]
      ay = arrows[,2]
    } else {
      stop('"arrows" must be FALSE, TRUE, an integer vector, or an n-by-2 matrix.')
    }

    good = which(is.finite(ax) & is.finite(ay))
    if (length(good) && arrow.size>0) {
      ax = ax[good]
      ay = ay[good]
      v = velocity(ax,ay)
      good = which(is.finite(v[,1]) & is.finite(v[,2]))
      if (length(good)) {
        len = arrow.size*diagonal
        ax = ax[good]
        ay = ay[good]
        dx = 0.5*len*v[good,1]
        dy = 0.5*len*v[good,2]
        graphics::arrows(ax-dx,ay-dy,ax+dx,ay+dy,length=0.08,...)
      }
    }
  }

  invisible(list(forward=p,backward=q,start=cbind(x=px,y=py)))
}
