#' Normalize vectors to unit length
#'
#' @description Compute the unit vectors for for a matrix of non-normalised vectors
#'
#' @param x m-element vector or n-by-m matrix \code{x[1:n,1:m]} representing n m-element vectors
#'
#' @return Returns a data type identical to \code{x}, but with normalised vectors. Zero-vectors are returned for vectors of length zero.
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{vectornorm}}
#'
#' @export

unitvector = function(x) {

  if (is.vector(x)) {

    return(x/max(.Machine$double.eps,sqrt(sum(x^2))))

  } else {

    return(x/pmax(.Machine$double.eps,sqrt(rowSums(x^2))))

  }

}
