#' Vector norm
#'
#' @description Compute the norm of a vector or a matrix of vectors
#'
#' @param x m-element vector or n-by-m matrix (x[1:n,1:m]) representing n m-element vectors
#'
#' @return Returns a scalar or an n-element vector with the vector norms
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{vectornorm}}
#'
#' @export

vectornorm = function(x) {

  if (is.vector(x)) {

    s = max(abs(x))
    if (s == 0) return(0)
    s * sqrt(sum((x / s)^2))

  } else {

    s = apply(abs(x), 1, max)
    s0 = s
    s0[s0 == 0] = 1

    s * sqrt(rowSums((x / s0)^2))

  }

}
