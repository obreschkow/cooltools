#' 2D rotation matrix
#'
#' @description Compute a 2D rotation matrix given a rotation angle
#'
#' @param angle rotation angle in radians (counter-clockwise)
#'
#' @return Returns a 2-by-2 anti-symmetric rotation matrix
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{rotation3}}
#'
#' @export

rotation2 = function(angle) {

  c = cos(angle)
  s = sin(angle)
  return(rbind(c(c,-s),c(s,c)))

}
