#' Planck CMB colour palette
#'
#' @importFrom grDevices rgb
#' @importFrom graphics rasterImage
#' @importFrom stats approx
#'
#' @description Generates color scale matching the one normally used to display the Planck CMB temperature map from -300uK to +300uK.
#'
#' @param n integer number of colors in the scale
#'
#' @return Returns an n-vector of RGB colour strings.
#'
#' @author Danail Obreschkow
#'
#'#' @examples
#' nplot()
#' rasterImage(rbind(planck.colors(1e3)),0,0,1,1)
#'
#' @export

planckcolors = function(n) {

  rlist = c(0,17,47,74,100,156,252,248,244,239,232,161,91)
  glist = c(6,58,113,167,218,227,236,208,181,132,87,49,13)
  blist = c(245,245,246,248,250,233,214,120,63,50,40,23,7)
  m = length(rlist)

  r = approx(seq(m),rlist,seq(1,m,length=n))$y
  g = approx(seq(m),glist,seq(1,m,length=n))$y
  b = approx(seq(m),blist,seq(1,m,length=n))$y

  return(rgb(r,g,b,maxColorValue=255))
}

