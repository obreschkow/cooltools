#' Radius of a point distribution
#'
#' @description
#' Computes a characteristic radius of a set of points about a centre.
#'
#' @param x Numeric vector or matrix. If a matrix is supplied, each row is
#'   interpreted as a point.
#' @param m Optional numeric vector of point masses or weights. If \code{NULL},
#'   equal weights are used.
#' @param x0 Optional numeric vector specifying the centre. If \code{NULL}, the
#'   weighted centre of mass is used.
#' @param type Character string specifying the radius definition. One of
#'   \code{"max"}, \code{"mean"} or \code{"median"}.
#'
#' @return
#' A scalar radius.
#'
#' @seealso \code{\link{vectornorm}}
#'
#' @export

distradius = function(x, m = NULL, x0 = NULL, type = c("max", "mean", "median")) {

  type = match.arg(type)

  x = as.matrix(x)
  if (ncol(x) == 1) x = t(x)

  n = nrow(x)

  if (is.null(m)) m = rep(1, n)

  if (length(m) != n)
    stop("m must have length equal to the number of rows in x")

  if (sum(m) == 0)
    return(NA_real_)

  if (is.null(x0))
    x0 = colSums(x * m) / sum(m)

  r = vectornorm(t(t(x) - as.vector(x0)))

  switch(
    type,
    max    = max(r),
    mean   = sum(m * r) / sum(m),
    median = {
      o = order(r)
      r = r[o]
      m = m[o]
      cm = cumsum(m) / sum(m)
      r[which(cm >= 0.5)[1]]
    }
  )
}
