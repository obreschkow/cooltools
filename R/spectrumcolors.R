#' Spectrum colour palette
#'
#' @importFrom grDevices rgb rainbow
#' @importFrom graphics rasterImage text abline
#' @importFrom stats approx
#'
#' @description Generates smooth rainbow color scale from red to purple, similar to \code{\link[grDevices]{rainbow}}, but with improved smoothness
#'
#' @param n integer number of colors in the scale
#' @param alpha alpha transparency value (0=fully transparent, 1=fully opaque)
#' @param rev logical flag indicating whether the ordering of the colors should be reversed
#'
#' @return Returns an n-vector of RGB colour strings.
#'
#' @author Danail Obreschkow
#'
#'#' @examples
#' nplot()
#' rasterImage(rbind(spectrumcolors(1e3)),0,0,1,0.5)
#' rasterImage(rbind(rainbow(1e3,end=5/6)),0,0.5,1,1)
#' text(0.5,0.25,'spectrum')
#' text(0.5,0.75,'rainbow')
#' abline(h=0.5)
#'
#' @export

spectrumcolors = function(n, alpha=1, rev=FALSE) {

  xin  = c(0,1.3,1.4,1.7,2.5,3,3.5,3.8,4.5,5)
  xout = c(0,1,1.1,1.4,2.9,3,3.4,3.65,4.7,5)

  i = seq(0.2,4.8,length=n)
  if (rev) i = rev(i)
  x = approx(xin,xout,i)$y

  r = pmin(0.9,2-x)*as.numeric(x<=2)+pmax(0,pmin(0.9,(0.65*x-2.3)))*as.numeric(x>=3)
  g = pmin(1,x)*as.numeric(x<2)+pmax(0,pmin(1,2.95-0.65*x))^2*as.numeric(x>=2)
  b = pmin(1,pmax(0,x-2))*as.numeric(x<4)+pmin(1,pmax(0,6-x))*as.numeric(x>=4)

  return(rgb(r,0.9*(-2*g^3+3*g^2),b,alpha=alpha))
}
