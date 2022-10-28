#' Stretch values to a custom range
#'
#' @description Shifts and stretches the values of a vector or array to a desired interval, while maintaining the shape of the input argument
#'
#' @param x vector or array
#' @param min minimum value
#' @param max maximum value
#' @param invert logical flag specifying whether the data should be inverted, such that the smallest input value maps to max and the largest input value maps to min.
#' @param gamma optional argument specifying a non-linear transformation x->x^gamma, if gamma>0, or x->1-(1-x)^(-gamma), if gamma<0.
#' @param na optional value specifying the value assigned to non-numbers (NA and NaN)
#'
#' @return vector/array of the same shape as \code{x}
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{lim}
#'
#' @export

stretch = function(x, min=0, max=1, invert=FALSE, gamma=NULL, na=NULL) {

  # stretch to unit interval
  x0 = min(x,na.rm = T)
  x1 = max(x,na.rm = T)
  if (x1==x0) {
    wx = 1
  } else {
    wx = x1-x0
  }
  x = (x-x0)/wx

  # inversion
  if (invert) x = 1-x

  # gamma correction
  if (!is.null(gamma)) {
    if (gamma>0) {
      x = x^gamma
    } else {
      x = 1-(1-x)^(-gamma)
    }
    x = pmin(1,pmax(0,x)) # to avoid numerical outliners
  }

  # stretch to desired interval
  x = min+(max-min)*x

  # NA replacement
  if (!is.null(na)) x[is.na(x)]=na

  # return result
  return(x)
}
