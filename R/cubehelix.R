#' Cube Helix colour palette
#'
#' @importFrom grDevices rgb
#'
#' @description Generate the Cube Helix colour palette, designed to appropriately display of intensity images, as the brightness increases monotonically when displayed in greyscale.
#'
#' @param n integer number of colours in the scale
#' @param r real number specifying the number of rotations of the helix over the scale
#' @param hue non-negative number specifying the colour intensity from grey (0) to normal (1) and over-saturated (>1)
#' @param gamma positive number specifying the relative importance of low vs high values
#' @param rev logical flag indicating whether the ordering of the colors should be reversed
#'
#' @return Returns an n-vector of RGB colour strings.
#'
#' @details This scheme was published by Green, D. A., 2011, "A colour scheme for the display of astronomical intensity images." Bulletin of the Astronomical Society of India, 39, 289.
#'
#' @author Danail Obreschkow
#'
#' @export

cubehelix = function (n, r = 1.5, hue = 1, gamma = 1, rev=FALSE) {
  M = cbind(c(-0.14861,-0.29227,1.97294),c(1.78277,-0.90649,0))
  lambda = seq(0, 1, length.out = n)
  l = rep(lambda^gamma, each = 3)
  phi = -2 * pi * (r * lambda)
  t = rbind(cos(phi), sin(phi))
  out = l + hue * l * (1 - l)/2 * (M %*% t)
  out = pmin(pmax(out, 0), 1)
  out = apply(out, 2, function(x) rgb(x[1], x[2], x[3]))
  if (rev) out = rev(out)
  return(out)
}
