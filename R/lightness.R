#' Change lightness of a color
#'
#' @importFrom grDevices col2rgb
#'
#' @param col is a color or vector/array of colors, specified as text (e.g. 'purple') or 7/9-character (e.g. '#A020F0')
#' @param light lightness value, according to a HSL scheme, between 0 and 1 or a vector/array thereof
#'
#' @return Returns a 9-character color or vector/array of 9-character colors.
#'
#' @examples
#'
#' # Generate different lightnesses of the same color
#' plot(runif(50),runif(50),pch=20,cex=10,col=lightness('purple',runif(50)))
#'
#' @seealso \code{\link{transparent}}
#'
#' @author Danail Obreschkow
#'
#' @export

lightness = function(col, light=0.5) {

  if (length(col)==1) {
    if (length(light)>1) {
      tmp = col
      col = light
      col[] = tmp
    }
  } else {
    if (length(light)==1) {
      light = rep(light,length(col))
    } else {
      if (length(col)!=length(light)) stop('col and light must have the same length (or be of length 1).')
    }
  }

  for (i in seq(length(col))) {
    rgb = col2rgb(col[i],alpha=T)/255
    if (light[i]<0.5) {
      rgb[1:3] = rgb[1:3]*light[i]*2
    } else {
      rgb[1:3] = 1-(1-rgb[1:3])*(1-light[i])*2
    }
    if (rgb[4]==1) {
      col[i] = rgb(rgb[1],rgb[2],rgb[3])
    } else {
      col[i] = rgb(rgb[1],rgb[2],rgb[3],rgb[4])
    }
  }

  return(col)

}
