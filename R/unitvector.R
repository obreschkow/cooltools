#' Normalize vectors to unit length
#'
#' @description
#' Computes unit vectors from one or more vectors.
#'
#' @param x Numeric vector or matrix. If a matrix is supplied, each row is
#'   interpreted as a vector.
#'
#' @return
#' An object of the same type and dimensions as \code{x}, with each vector
#' normalized to unit length. Zero vectors are returned unchanged.
#'
#' @seealso \code{\link{vectornorm}}
#'
#' @export

unitvector = function(x) {

  n = vectornorm(x)

  if (is.vector(x)) {

    if (n > 0) x / n else x

  } else {

    n[n == 0] = 1
    x / n

  }

}
