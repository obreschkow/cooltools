#' Convert complex numbers to color
#'
#' @importFrom grDevices hsv
#'
#' @description Converts a complex number (or a vector/array thereof) into a color-string, such that brightness represents the complex amplitude and hue represents the complex phase.
#'
#' @param z complex number or vector/array of complex numbers
#' @param max value of the complex module that corresponds to full brightness; if not given, this is set equal to the maximum complex amplitude in z.
#' @param hue hue value of positive reals
#' @param saturation saturation value of colors
#' @param gamma positive number to adjust the non-linearity of the color scale (1=linear)
#'
#' @return Returns a single color or vector/array of colors
#'
#' @author Danail Obreschkow
#'
#' @export

cmplx2col = function(z, max=NULL, hue=0, saturation=1, gamma=1) {
  amp = Mod(z)
  phi = Arg(z)
  if (is.null(max)) max = max(amp)
  max = max(.Machine$double.xmin,max)
  col = hsv(((phi/2/pi)+hue)%%1, saturation, pmax(0,pmin(1,amp/max))^gamma)
  if (length(dim(z))>1) col = array(col,dim(z))
  return(col)
}
