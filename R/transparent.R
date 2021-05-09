#' Add transparency to a color
#'
#' @importFrom grDevices col2rgb
#'
#' @param col is a color or vector/array of colors, specified as text (e.g. 'purple') or 7/9-character (e.g. '#A020F0')
#' @param alpha transparency value between 0 and 1 or a vector/array thereof
#'
#' @return Returns a 9-character color or vector/array of 9-character colors.
#'
#' @examples
#'
#' # Add different transparencies of the same color
#' plot(runif(50),runif(50),pch=20,cex=10,col=transparent('purple',runif(50)))
#'
#' # Add the same transparency to different colors
#' plot(runif(50),runif(50),pch=20,cex=10,col=transparent(rainbow(50)))
#'
#' @seealso \code{\link{lightness}}
#'
#' @author Danail Obreschkow
#'
#' @export

transparent = function(col, alpha=0.5) {

  if (length(col)==1) {
    if (length(alpha)>1) {
      tmp = col
      col = alpha
      col[] = tmp
    }
  } else {
    if (length(alpha)==1) {
      alpha = rep(alpha,length(col))
    } else {
      if (length(col)!=length(alpha)) stop('col and alpha must have the same length (or be of length 1).')
    }
  }

  for (i in seq(length(col))) {
    rgb = col2rgb(col[i])/255
    col[i] = rgb(rgb[1],rgb[2],rgb[3],alpha[i])
  }
  return(col)

}
