#' Circularly shift each dimension of an array
#'
#' @description Circulates each dimension of an array. This routine is identical to \code{\link[pracma]{circshift}}, but works with arrays up to 5 dimensions.
#'
#' @param x vector or array (up to rank 5)
#' @param s scalar, if x is a vector, or a vector of length matching the rank of x, if x is an array
#'
#' @return Returns a vector or array of the same shape as x.
#'
#' @author Danail Obreschkow
#'
#' @export

cshift = function(x,s) {

  if (is.null(x)) return(x)

  if (is.vector(x) && length(s) == 1) {

    n = length(x)
    s = s%%n
    x = x[(1:n-s-1)%%n+1]

  } else if (is.array(x)) {

    if (length(dim(x))>5) stop("x must be an array of rank 1-5.")
    if (length(dim(x))!=length(s)) stop("Length of s must be equal to the number of dimensions of x.")

    n = dim(x)
    s = s%%n
    d = length(n)

    if (d==1) {
      x = x[(1:n-s-1)%%n+1]
    } else if (d==2) {
      x = x[(1:n[1]-s[1]-1)%%n[1]+1,(1:n[2]-s[2]-1)%%n[2]+1]
    } else if (d==3) {
      x = x[(1:n[1]-s[1]-1)%%n[1]+1,(1:n[2]-s[2]-1)%%n[2]+1,(1:n[3]-s[3]-1)%%n[3]+1]
    } else if (d==4) {
      x = x[(1:n[1]-s[1]-1)%%n[1]+1,(1:n[2]-s[2]-1)%%n[2]+1,(1:n[3]-s[3]-1)%%n[3]+1,(1:n[4]-s[4]-1)%%n[4]+1]
    } else if (d==5) {
      x = x[(1:n[1]-s[1]-1)%%n[1]+1,(1:n[2]-s[2]-1)%%n[2]+1,(1:n[3]-s[3]-1)%%n[3]+1,(1:n[4]-s[4]-1)%%n[4]+1,(1:n[5]-s[5]-1)%%n[5]+1]
    }

  }

  return(x)

}
