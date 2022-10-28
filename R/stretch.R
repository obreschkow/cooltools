#' Stretch values to a custom range
#'
#' @description Linearly shifts and stretches the values of a vector or array to a desired interval, while maintaining the shape of the input argument
#'
#' @param x vector or array
#' @param min minimum value
#' @param max maximum value
#' @param na optional value specifying the value assigned to non-numbers (NA and NaN)
#'
#' @return vector/array of the same shape as \code{x}
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{lim}
#'
#' @export

stretch = function(x, min=0, max=1, na=NULL) {
  x0 = min(x,na.rm = T)
  x1 = max(x,na.rm = T)
  if (x1==x0) {
    f = max-min
  } else {
    f = (max-min)/(x1-x0)
  }
  x = min+f*(x-x0)
  if (!is.null(na)) x[is.na(x)]=na
  return(x)
}
