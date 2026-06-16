#' Draw streamlines of a two-dimensional vector field
#'
#' Draws streamlines of a planar vector field \eqn{f(x,y)} by integrating
#' forward and backward from a set of starting points. Starting points can be
#' provided as a single integer, a length-2 integer vector, or an \eqn{n \times 2}
#' matrix of coordinates.
#'
#' @param f A function of the form `f(x, y)` returning an \eqn{n \times 2}
#'   matrix of vector components, where the first column is the x-component and
#'   the second column is the y-component.
#' @param xlim Numeric vector of length 2 giving the x-limits of the plot.
#' @param ylim Numeric vector of length 2 giving the y-limits of the plot.
#' @param points Starting points for the streamlines. This can be:
#'   \itemize{
#'     \item a single integer, interpreted as the number of quasi-random starting points;
#'     \item a numeric vector of length 2, interpreted as the numbers of starting
#'       points in x- and y-direction;
#'     \item a numeric matrix with 2 columns, giving explicit starting coordinates.
#'   }
#' @param nsteps Integer giving the number of integration steps in each direction.
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
#' @param add Logical; if `FALSE` a new plot is created, otherwise streamlines are
#'   added to the existing plot.
#' @param ... Further graphical parameters passed to [graphics::lines()] and
#'   [graphics::arrows()].
#'
#' @details
#' The integration uses a fixed step length based on the plot diagonal,
#' \deqn{d = \sqrt{dx^2 + dy^2} / nsteps,}
#' where `dx = diff(range(xlim))` and `dy = diff(range(ylim))`.
#' At each step, only the direction of the vector field is used, via
#' [unitvector()], so the result shows streamlines of the field rather than
#' trajectories with speed information.
#'
#' If `add = FALSE`, a new plot is initialized using [plot()].
#'
#' Arrows are drawn along the local field direction. If `arrows = TRUE`, each
#' arrow is centered on the streamline starting point. If `arrows` is a matrix
#' of coordinates, the arrows are placed at those coordinates, regardless of
#' whether they lie exactly on a streamline.
#'
#' @return
#' Invisibly returns a list with components:
#' \itemize{
#'   \item `forward`: array of forward-integrated coordinates;
#'   \item `backward`: array of backward-integrated coordinates;
#'   \item `start`: matrix of starting coordinates.
#' }
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
                       arrows = FALSE, add = FALSE, ...) {

  if (!is.function(f)) {
    stop('"f" must be a function.')
  }
  if (!is.numeric(xlim) || length(xlim) != 2) {
    stop('"xlim" must be a numeric vector of length 2.')
  }
  if (!is.numeric(ylim) || length(ylim) != 2) {
    stop('"ylim" must be a numeric vector of length 2.')
  }
  if (!is.numeric(nsteps) || length(nsteps) != 1 || nsteps < 2) {
    stop('"nsteps" must be a single integer >= 2.')
  }
  nsteps = as.integer(nsteps)

  # determine starting points
  if (length(points) == 1 && is.null(dim(points))) {

    r = rng(
      function(x) rep(1, nrow(x)),
      points,
      min = c(xlim[1], ylim[1]),
      max = c(xlim[2], ylim[2]),
      quasi = TRUE
    )$x
    px = r[, 1]
    py = r[, 2]

  } else if (length(points) == 2 && is.null(dim(points))) {

    rx = midseq(xlim[1], xlim[2], points[1])
    ry = midseq(ylim[1], ylim[2], points[2])
    grid = expand.grid(rx, ry)
    px = grid[[1]]
    py = grid[[2]]

  } else if (length(dim(points)) == 2 && dim(points)[2] == 2) {

    px = points[, 1]
    py = points[, 2]

  } else {
    stop('unknown format of input argument "points"')
  }

  np = length(px)

  # integrate forward and backward
  d = sqrt(diff(range(xlim))^2 + diff(range(ylim))^2) / nsteps

  p = q = array(NA_real_, c(np, 2, nsteps))
  p[, 1, 1] = q[, 1, 1] = px
  p[, 2, 1] = q[, 2, 1] = py

  for (i in seq_len(nsteps - 1)) {
    v = unitvector(f(p[, 1, i], p[, 2, i]))
    p[, , i + 1] = p[, , i] + v[, 1:2, drop = FALSE] * d

    v = unitvector(f(q[, 1, i], q[, 2, i]))
    q[, , i + 1] = q[, , i] - v[, 1:2, drop = FALSE] * d
  }

  # draw plot if requested
  if (!add) plot(NULL, xlab = "", ylab = "", xlim = xlim, ylim = ylim)

  # draw streamlines
  for (i in seq_len(np)) {
    coordx = c(rev(q[i, 1, ]), p[i, 1, 2:nsteps])
    coordy = c(rev(q[i, 2, ]), p[i, 2, 2:nsteps])
    lines(coordx, coordy, ...)
  }

  # draw arrows if requested
  draw_one_arrow = function(x0, y0, vx, vy, len = 1.5 * d, ...) {
    v = unitvector(cbind(vx, vy))
    if (!is.finite(v[1, 1]) || !is.finite(v[1, 2])) return(invisible(NULL))
    dx = 0.5 * len * v[1, 1]
    dy = 0.5 * len * v[1, 2]
    graphics::arrows(x0 - dx, y0 - dy, x0 + dx, y0 + dy,
                     length = 0.08, ...)
    invisible(NULL)
  }

  if (!identical(arrows, FALSE)) {

    if (identical(arrows, TRUE)) {

      v0 = unitvector(f(px, py))
      for (i in seq_len(np)) {
        draw_one_arrow(px[i], py[i], v0[i, 1], v0[i, 2], ...)
      }

    } else if (is.numeric(arrows) && is.null(dim(arrows))) {

      ii = unique(as.integer(arrows))
      ii = ii[is.finite(ii) & ii >= 1 & ii <= np]
      if (length(ii) > 0) {
        v0 = unitvector(f(px[ii], py[ii]))
        for (j in seq_along(ii)) {
          i = ii[j]
          draw_one_arrow(px[i], py[i], v0[j, 1], v0[j, 2], ...)
        }
      }

    } else if (is.matrix(arrows) && ncol(arrows) == 2) {

      ax = arrows[, 1]
      ay = arrows[, 2]
      v0 = unitvector(f(ax, ay))
      for (i in seq_len(nrow(arrows))) {
        draw_one_arrow(ax[i], ay[i], v0[i, 1], v0[i, 2], ...)
      }

    } else {
      stop('"arrows" must be FALSE, TRUE, an integer vector, or an n-by-2 matrix.')
    }
  }

  invisible(list(
    forward = p,
    backward = q,
    start = cbind(x = px, y = py)
  ))
}
