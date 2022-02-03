#' Generate histogram coordinates from mid points
#'
#' @description Converts the mid-point x-values and mean densities of binned data into (x,y)-coordinates of a histogram.
#'
#' @param x n-vector giving the bin mid-points
#' @param y n-vector giving bin values
#' @param yleft optional value specifying the value at the left edge
#' @param yright optional value specifying the value at the right edge
#'
#' @return (2n+2)-by-2 matrix of (x,y)-coordinates to draw histogram as a connected line
#'
#' @author Danail Obreschkow
#'
#' @examples
#' x = seq(5)
#' y = sin(x)
#' plot(x,y,xlim=c(0,6))
#' lines(histcoord(x,y))
#'
#' @export

histcoord = function(x,y,yleft=0,yright=0) {
  n = length(x)
  x = c(x[1]-(x[2]-x[1])/2, (x[2:n]+x[1:(n-1)])/2, x[n]+(x[n]-x[n-1])/2)
  data.frame(x = rep(x,each=2),
             y = c(yleft,rep(y,each=2),yright))
}


