#' Crop values of vector or array to a custom range
#'
#' @description limits the values of a vector or array to a desired interval, while keeping the shape of the vector/array
#'
#' @param x vector or array
#' @param min minimum value
#' @param max maximum value
#'
#' @return vector/array of the same shape as \code{x}
#'
#' @author Danail Obreschkow
#'
#' @export lim

lim = function(x,min=0,max=1) {
  x[x<min] = min
  x[x>max] = max
  return(x)
}
