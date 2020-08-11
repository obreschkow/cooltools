#' 3D rotation matrix
#'
#' @description Compute a 3D rotation matrix given an axis and an angle
#'
#' @param u 3-vector specifying the rotation axis
#' @param angle rotation angle in radians; if not given, the norm of the vector \code{u} is interpreted as the angle
#'
#' @return Returns a 3-by-3 rotation matrix
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{rotation2}}
#'
#' @export

rotation3 = function(u, angle=NULL) {

  n = length(u)

  if (n==3) {

    unorm = sqrt(sum(u^2))

    if (unorm==0) {

        R = diag(3)

    } else {

      if (is.null(angle)) angle = unorm
      u = u/unorm
      c = cos(angle)
      s = sin(angle)
      R = rbind(c(c+u[1]^2*(1-c), u[1]*u[2]*(1-c)-u[3]*s, u[1]*u[3]*(1-c)+u[2]*s),
                c(u[2]*u[1]*(1-c)+u[3]*s, c+u[2]^2*(1-c), u[2]*u[3]*(1-c)-u[1]*s),
                c(u[3]*u[1]*(1-c)-u[2]*s, u[3]*u[2]*(1-c)+u[1]*s, c+u[3]^2*(1-c)))
    }

    return(R)

  } else {

    stop('u must be a 3-element vector')

  }

}
