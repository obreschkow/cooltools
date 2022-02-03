#' Crop values of vector or array to a custom range
#'
#' @description limits the values of a vector or array to a desired interval, while keeping the shape of the vector/array
#'
#' @param x vector or array
#' @param min minimum value
#' @param max maximum value
#' @param clip optional value specifying the value assigned to clipped data, e.g. \code{clip=NA}
#' @param na optional value specifying the value assigned to non-numbers (NA and NaN)
#'
#' @return vector/array of the same shape as \code{x}
#'
#' @author Danail Obreschkow
#'
#' @export

lim = function(x, min=0, max=1, clip=NULL, na=NULL) {
  if (!is.null(clip)) s = x<min | x>max
  x[x<min] = min
  x[x>max] = max
  if (!is.null(clip)) x[s]=clip
  if (!is.null(na)) x[is.na(x)]=na
  return(x)
}
