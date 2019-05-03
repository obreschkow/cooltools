#' Add transparency to a color
#'
#' @importFrom grDevices col2rgb
#'
#' @param col is a color or vector of colors, specified as text (e.g. 'purple') or 7/9-character (e.g. '#A020F0')
#' @param alpha tranparency value between 0 and 1
#'
#' @return Returns a 9-character color or vector of 9-character colors.
#'
#' @examples
#' col = transparent('purple',0.5)
#'
#' @author Danail Obreschkow
#'
#' @export transparent

transparent = function(col,alpha=0.3) {
  rgb = col2rgb(col)/255
  return(rgb(rgb[1],rgb[2],rgb[3],alpha))
}
