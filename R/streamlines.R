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
#' @param width.power Exponent relating line width to local field magnitude.
#'   Zero (default) gives constant width; negative values emphasize slow flow.
#' @param width.range Two positive limits for the width multiplier relative to
#'   the supplied `lwd` (or `par("lwd")`).
#' @param dt Optional positive time interval between markers. Time is measured
#'   in coordinate units divided by the units of `f`. NULL disables markers.
#' @param marker.cex Size of equal-time dots, relative to the default point size.
#' @param max.markers Maximum number of time markers per direction per path.
#'   If exceeded, markers are truncated and a warning is issued.
#' @param ... Further graphical parameters passed to [graphics::lines()] and
#'   [graphics::arrows()]. Graphical parameters are shared by all streamlines.
#'
#' @details
#' The integration uses a fixed Euler step length based on the plot diagonal,
#' \deqn{d = \sqrt{dx^2 + dy^2} / nsteps,}
#' where `dx = diff(range(xlim))` and `dy = diff(range(ylim))`.
#' Spatial integration uses only the direction of the vector field. Optional
#' widths encode speed relative to the median plotted segment speed. Optional
#' markers integrate time using dt = ds/speed at each Euler step, independently
#' of plotting subsampling. These represent travel times in a frozen field.
#' Forward and backward integration are vectorized together, and terminated
#' paths are excluded from subsequent field evaluations.
#'
#' Plotting vertices are uniformly subsampled along each valid path when
#' necessary, preserving its starting and final valid points. All streamlines
#' are drawn in a single graphics call, separated by missing coordinates,
#' unless variable widths are requested, in which case segments are drawn.
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
#' Unused entries after termination are `NA`. When `dt` is supplied,
#' `time.forward` and `time.backward` contain signed elapsed times (matrices
#' with one row per starting point).
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
                       nplot = 200, width.power = 0, width.range = c(0.25,4),
                       dt = NULL, marker.cex = 0.4, max.markers = 1000, ...) {

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
  if (!is.numeric(width.power) || length(width.power)!=1 || !is.finite(width.power))
    stop('"width.power" must be a finite scalar.')
  if (!is.numeric(width.range) || length(width.range)!=2 ||
      any(!is.finite(width.range)) || any(width.range<=0) || diff(width.range)<0)
    stop('"width.range" must contain two ordered positive finite values.')
  if (!is.null(dt) && (!is.numeric(dt) || length(dt)!=1 || !is.finite(dt) || dt<=0))
    stop('"dt" must be NULL or a positive finite scalar.')
  if (!is.numeric(marker.cex) || length(marker.cex)!=1 || !is.finite(marker.cex) || marker.cex<=0)
    stop('"marker.cex" must be positive and finite.')
  if (!is.numeric(max.markers) || length(max.markers)!=1 || !is.finite(max.markers) ||
      max.markers<1 || max.markers!=floor(max.markers))
    stop('"max.markers" must be a positive integer.')
  timing = !is.null(dt)
  measure = timing || width.power!=0
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
  velocity = function(x,y, magnitude=FALSE) {
    v = f(x,y)
    if (!is.matrix(v) || !is.numeric(v) || nrow(v)!=length(x) || ncol(v)!=2)
      stop('"f" must return a numeric matrix with one row per point and two columns.')
    good = which(is.finite(v[,1]) & is.finite(v[,2]) & (v[,1]!=0 | v[,2]!=0))
    out = matrix(NA_real_,length(x),2)
    if (length(good)) out[good,] = unitvector(v[good,,drop=FALSE])
    if (magnitude) {
      speed = rep(NA_real_,length(x))
      if (length(good)) {
        scale = pmax(abs(v[good,1]),abs(v[good,2]))
        speed[good] = scale*sqrt((v[good,1]/scale)^2+(v[good,2]/scale)^2)
      }
      return(list(v=out,speed=speed))
    }
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

  if (measure) {
    tp = tq = matrix(NA_real_,np,nsteps)
    tp[,1] = tq[,1] = 0
    elapsed = numeric(2*np)
  }

  for (i in seq_len(nsteps-1L)) {
    if (!length(active)) break
    v = velocity(pos[active,1],pos[active,2],measure)
    if (measure) {
      speed = v$speed
      v = v$v
    }
    good = which(is.finite(v[,1]) & is.finite(v[,2]))
    if (measure) speed = speed[good]
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
    if (measure) {
      elapsed[active] = elapsed[active]+d/speed[good]
      if (length(forward)) tp[forward,i+1L] = elapsed[forward]
      if (length(backward)) tq[backward-np,i+1L] = elapsed[backward]
    }
  }

  # Draw plot if requested
  if (!add) plot(NULL,xlab='',ylab='',xlim=xlim,ylim=ylim)

  # Subsample each valid path, preserving both endpoints
  plotindex = function(n) {
    if (n<=nplot) seq_len(n) else unique(round(seq(1,n,length.out=nplot)))
  }
  coords = vector('list',np)
  times = vector('list',np)
  for (i in seq_len(np)) {
    kp = plotindex(last[i])
    kq = rev(plotindex(last[np+i]))
    coords[[i]] = cbind(c(q[i,1,kq],p[i,1,kp[-1L]],NA_real_),
                        c(q[i,2,kq],p[i,2,kp[-1L]],NA_real_))
    if (measure) times[[i]] = c(-tq[i,kq],tp[i,kp[-1L]],NA_real_)
  }
  if (np) {
    xy = do.call(rbind,coords)
    if (width.power==0) {
      graphics::lines(xy[,1],xy[,2],...)
    } else {
      tt = unlist(times,use.names=FALSE)
      ds = sqrt(rowSums((xy[-1,,drop=FALSE]-xy[-nrow(xy),,drop=FALSE])^2))
      speed = ds/diff(tt)
      k = which(is.finite(speed) & speed>0)
      if (length(k)) {
        ref = stats::median(speed[k])
        mult = pmin(width.range[2],pmax(width.range[1],(speed[k]/ref)^width.power))
        args = list(...)
        lwd = if (is.null(args$lwd)) par('lwd') else args$lwd
        args$lwd = lwd*mult
        do.call(graphics::segments,c(list(x0=xy[k,1],y0=xy[k,2],
                                         x1=xy[k+1L,1],y1=xy[k+1L,2]),args))
      }
    }
  }

  # Equal-time markers interpolated on the full-resolution paths
  if (timing && np) {
    dots = vector('list',2*np)
    truncated = FALSE
    for (j in seq_len(2*np)) {
      i = if (j<=np) j else j-np
      k = seq_len(last[j])
      t = if (j<=np) tp[i,k] else tq[i,k]
      a = if (j<=np) p else q
      valid = which(is.finite(t) & !duplicated(t))
      if (length(valid)<2) next
      t = t[valid]
      k = k[valid]
      n = floor(t[length(t)]/dt)
      if (n>max.markers) truncated = TRUE
      n = min(n,max.markers)
      if (n<1) next
      at = seq_len(n)*dt
      dots[[j]] = cbind(stats::approx(t,a[i,1,k],xout=at)$y,
                        stats::approx(t,a[i,2,k],xout=at)$y)
    }
    xy = rbind(cbind(px,py),do.call(rbind,dots))
    args = list(...)
    graphics::points(xy[,1],xy[,2],pch=16,cex=marker.cex,
                     col=if (is.null(args$col)) par('col') else args$col)
    if (truncated) warning('Equal-time markers truncated at max.markers per direction.')
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

  out = list(forward=p,backward=q,start=cbind(x=px,y=py))
  if (timing) {
    out$time.forward = tp
    out$time.backward = -tq
  }
  invisible(out)
}
