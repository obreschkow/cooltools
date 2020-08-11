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

    return(sqrt(sum(x^2)))

  } else {

    return(sqrt(rowSums(x^2)))

  }

}
