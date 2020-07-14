#' Convert complex numbers to color
#'
#' @importFrom grDevices hsv
#'
#' @description Converts a complex number (or a vector/array thereof) into a color-string, such that brightness represents the complex amplitude and hue represents the complex phase.
#'
#' @param z complex number or vector/array of complex numbers
#' @param max maximum
#' @param hue hue value of positive reals
#' @param saturation saturation value of colors
#'
#' @return Returns a single color or vector/array of colors
#'
#' @author Danail Obreschkow
#'
#' @export

cmplx2col = function(z, max=1, hue=0, saturation=1) {
  amp = Mod(z)
  phi = Arg(z)
  col = hsv(((phi/2/pi)+hue)%%1, saturation, pmax(0,pmin(1,amp/max)))
  if (length(dim(z))>1) col = array(col,dim(z))
  return(col)
}
