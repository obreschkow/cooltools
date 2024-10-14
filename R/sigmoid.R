#' Sigmoid function
#'
#' @importFrom pracma erf
#' @importFrom graphics curve lines
#'
#' @description Returns S-shaped functions, monotonically increasing between two constants with a point symmetry at the half-way point.
#'
#' @param x real argument between -Inf and +Inf, can be a vector
#' @param type number or character string, specifying the type of function. The following expressions vary between -1 and +1 with a slope of 1 at the symmetry x=0.\cr
#' \code{tanh} (1): hyperbolic tangent function, \code{tanh(x)}, which becomes the logistic function, \code{1/(1+exp(-x))}, if all arguments kept at default.\cr
#' \code{atan} (2): arctangent function, \code{2/pi*atan(pi/2*x)}\cr
#' \code{err} (3): error function, \code{erf(sqrt(pi)/2*x)}\cr
#' \code{sqrt} (4): algebraic function with root, \code{x/sqrt(1+x^2)}\cr
#' \code{abs} (5): algebraic function with absolute value, \code{x/(1+abs(x))}\cr
#' \code{gd} (6): Gudermannian function, \code{4/pi*atan(tanh(pi/4*x))} \cr
#' @param fmin asymptotic function minimum at \code{x=-Inf}
#' @param fmax asymptotic function maximum at \code{x=+Inf}
#' @param xmid point of symmetry, where the function value is \code{(fmin+fmax)/2}
#' @param slope slope of the function at \code{x=xmid}
#'
#' @return Returns a vector with the same number of elements as \code{x}
#'
#' @author Danail Obreschkow
#'
#' @examples
#' col = c('red','orange','#cccc00','#00bb00','blue','purple')
#' for (i in seq(6)) {
#'   graphics::curve(sigmoid(x,i),-3,3,add=i>1,col=col[i],ylab='sigmoid(x)')
#' }
#' graphics::lines(c(-1.5,1.5),c(-1,2),lty=2)
#'
#' @export

sigmoid = function(x, type = 1, fmin = 0, fmax = 1, xmid = 0, slope = 1) {

  if (is.numeric(type)) {
    if (type<1 | type>6) stop('unknown value for argument type')
    type = c('tanh','atan','err','sqrt','abs','gd')[type]
  }
  type = tolower(type)

  x = (x-xmid)*slope/(fmax-fmin)*2

  if (type=='tanh') {
    f = tanh(x)
  } else if (type=='atan') {
    f = 2/pi*atan(pi/2*x)
  } else if (type=='err') {
    f = pracma::erf(sqrt(pi)/2*x)
  } else if (type=='sqrt') {
    f = x/sqrt(1+x^2)
  } else if (type=='abs') {
    f = x/(1+abs(x))
  } else if (type=='gd') {
    f = 4/pi*atan(tanh(pi/4*x))
  } else {
    stop('unknown value for argument type')
  }

  return((f+1)/2*(fmax-fmin)+fmin)

}
