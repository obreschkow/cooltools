#' Gaussian blur for arrays of arbitrary dimension
#'
#' Smooth a numeric array of arbitrary dimension with a separable Gaussian
#' kernel. This function is conceptually similar to \code{gblur} from the
#' \pkg{EBImage} package (which is limited to 2D), but works for arrays of
#' any dimension.
#'
#' @param x A numeric array to be smoothed.
#' @param sigma A non-negative numeric scalar or numeric vector specifying the
#'   standard deviation(s) of the Gaussian kernel in pixel units. If a single
#'   value is provided, it is used for all dimensions.
#' @param boundary A character string specifying the boundary handling, or a
#'   character vector of length equal to the number of dimensions of `x`.
#'   Possible values are `"none"`, `"circular"`, and `"replicate"`. If a single
#'   value is provided, it is used for all dimensions.
#'
#' @return
#' A numeric array with the same dimensions as `x`, containing the blurred
#' values.
#'
#' @details
#' The Gaussian kernel is separable, so smoothing is performed via successive
#' 1D convolutions along each dimension.
#'
#' This function is similar in spirit to \code{gblur} from \pkg{EBImage}, but
#' generalises naturally to arrays of arbitrary dimension.
#'
#' Boundary handling can be controlled via `boundary`:
#' \itemize{
#'   \item `"none"` uses centered filtering without padding and therefore
#'   returns `NA` values near array boundaries.
#'   \item `"circular"` applies periodic wrap-around boundary conditions.
#'   \item `"replicate"` extends the array by repeating the edge values.
#' }
#'
#' For finite arrays, the Gaussian kernel is truncated as needed so that its
#' length does not exceed the array extent along any dimension.
#'
#' @importFrom utils head
#'
#' @export
blur = function(x, sigma, boundary = "replicate") {

  if (!is.array(x)) {
    stop("x must be an array.")
  }
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  dims = dim(x)
  d = length(dims)

  if (length(sigma) == 1) {
    sigma = rep(sigma, d)
  }
  if (length(sigma) != d) {
    stop("sigma must have length 1 or match the number of dimensions of x.")
  }
  if (any(!is.finite(sigma)) || any(sigma < 0)) {
    stop("sigma must contain non-negative finite values.")
  }

  if (length(boundary) == 1) {
    boundary = rep(boundary, d)
  }
  if (length(boundary) != d) {
    stop("boundary must have length 1 or match the number of dimensions of x.")
  }
  if (!all(boundary %in% c("none", "circular", "replicate"))) {
    stop("boundary must contain only 'none', 'circular', or 'replicate'.")
  }

  gauss_kernel = function(s, n) {
    if (s == 0) return(1)
    r = ceiling(4 * s)
    r = min(r, floor((n - 1) / 2))
    k = seq(-r, r)
    w = exp(-0.5 * (k / s)^2)
    w / sum(w)
  }

  blur1d = function(v, w, mode) {
    k = length(w)
    r = (k - 1) %/% 2

    if (mode == "none") {
      return(as.numeric(stats::filter(v, w, sides = 2)))
    }

    if (mode == "circular") {
      vp = c(tail(v, r), v, utils::head(v, r))
    } else if (mode == "replicate") {
      vp = c(rep(v[1], r), v, rep(v[length(v)], r))
    } else {
      stop("Invalid boundary mode.")
    }

    y = stats::filter(vp, w, sides = 2)
    as.numeric(y[(r + 1):(r + length(v))])
  }

  filter_along_dim = function(a, w, i, mode) {
    perm = c(i, setdiff(seq_along(dim(a)), i))
    a = aperm(a, perm)
    dima = dim(a)

    m = matrix(a, nrow = dima[1], ncol = prod(dima[-1]))
    m = apply(m, 2, function(v) blur1d(v, w, mode))
    m = array(m, dim = dima)

    aperm(m, order(perm))
  }

  y = x

  for (i in seq_len(d)) {
    w = gauss_kernel(sigma[i], dims[i])
    y = filter_along_dim(y, w, i, boundary[i])
  }

  return(y)
}
