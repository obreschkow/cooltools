#' Find contour levels of a d-dimensional density field
#'
#' @importFrom pracma meshgrid
#' @importFrom cubature cuhre
#' @importFrom graphics image contour
#' @importFrom stats approxfun optim optimize
#' @importFrom MASS mvrnorm
#'
#' @description Given a vector d-dimensional vector/array \code{f} or function \code{f(x)} of a d-element vector \code{x}, this routine evaluates the value l, such that the sum/integral of f over the domain f>l makes up a fraction p of the total sum/integral of f. The main purpose of this routine is to determine the iso-contour levels of a likelihood or density function, containing a cumulative probability-mass p.
#'
#' @param f either a d-dimensional vector/array or a function \code{f(x)} of a d-element vector \code{x}. There is no need for \code{f} to be normalized.
#' @param p vector of probabilities
#' @param xmin,xmax (only used if \code{f} is a function) vectors with of lower and upper limits of \code{x}, defining the domain on which the function \code{f(x)} is evaluated. Outside this domain, \code{f} is assumed to vanish. These limits should be chosen as narrow as possible for the algorithm to converge quickly.
#' @param neval (only used if \code{f} is a function) maximum number of function evaluations in numerical integrals
#' @param napprox (only used if \code{f} is a function) number of points used interpolate the cumulative probability density. If set to 0, no interpolation is used.
#' @param ... (only used if \code{f} is a function) additional parameters to be passed to the function f.
#'
#' @return Returns a vector of levels l, which has the same length as p.
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{dpqr}}
#'
#' @examples
#'
#' ## f(x) is a 1D PDF
#' # compute 1-sigma and 2-sigma contour levels of a normal distribution, i.e.
#' # the values l, such that int_{dnorm(x)>l} dnorm(x) dx = p (=68.3%, 95.4%).
#' l = contourlevel(dnorm,xmin=-10,xmax=10,napprox=0)
#' print(l)
#'
#' # compare these values to the solutions dnorm(1), dnorm(2)
#' print(dnorm(c(1,2)))
#'
#'
#' ## f(x) is a 2D likelihood function
#' # Produce 20%, 40%, 60% and 80% iso contours on the 2D likelihood function f(x)
#' f = function(x) cos(2*x[1]-x[2]-1)^2*exp(-x[1]^2-x[2]^2-x[1]*x[2])
#' p = c(0.2,0.4,0.6,0.8) # cumulative probability
#' l = contourlevel(f,p,c(-5,-5),c(5,5)) # values l, such that int_{f>l}(f(x)dx)=p int(f(x)*dx)
#'
#' # Plot function and contours at the levels l
#' x = seq(-3,3,length=200)
#' m = pracma::meshgrid(x)
#' z = array(Vectorize(function(x,y) f(c(x,y)))(m$Y,m$X),dim(m$X))
#' image(x,x,z,col=terrain.colors(100))
#' contour(x,x,z,levels=l,add=TRUE,labels=sprintf('%.0f%%',p*100),labcex=0.7)
#'
#'
#' ## f is a 20-by20 array representing a gridded pointset
#' # produce a set of 1000 points in 2D, drawn from a 2D normal distribution
#' set.seed(1)
#' x = MASS::mvrnorm(n=1000, mu=c(0,0), matrix(c(3,1,1,2),2,2))
#'
#' # grid these points onto a regular 20-by-20 grid
#' g = griddata2(x, xlim=c(-6,6), ylim=c(-6,6))
#'
#' # find 1-sigma and 2-sigma contour levels and draw contours at these levels
#' l = contourlevel(g$n)
#' plot(x, xlim=g$xlim, ylim=g$ylim, pch=20, cex=0.5)
#' contour(g$x,g$y,g$n,levels=l,add=TRUE,col='red',lwd=c(2,1),labels=NA)
#'
#' @export

contourlevel = function(f, p=c(0.6826895,0.9544997), xmin=NULL, xmax=NULL, neval=1e4, napprox=10, ...) {

  if (is.function(f)) {

    return(.contourlevel.function(f, p, xmin, xmax, neval, napprox,...))

  } else if (is.array(f) | is.vector(f)) {

    f = as.vector(f)
    return(.contourlevel.vector(f, p))

  } else {

    stop('f must be a vector, array or a function of a d-element vector')

  }
}

.contourlevel.function = function(f, p, xmin=NULL, xmax=NULL, neval=1e4, napprox=10, ...) {

  if (is.null(xmin)) stop('xmin must be specified if f is a function.')
  if (is.null(xmax)) stop('xmax must be specified if f is a function.')

  f.max = stats::optim((xmin+xmax)/2,f,lower=xmin,upper=xmax,method='L-BFGS-B',
                control=list(fnscale=xmin-xmax), ...)$value

  f.norm = cubature::cuhre(f,1,xmin,xmax,relTol=1e-8,...)$integral

  f.norm.partial = function(l) {
    integrand = function(x) f(x,...)*as.numeric(f(x,...)>=l)
    return(cubature::cuhre(integrand,1,xmin,xmax,maxEval=neval)$integral)
  }

  if (napprox<=0) {
    f.norm.partial.approx = f.norm.partial
  } else {
    w = seq(0,f.max,length=napprox)
    f.norm.partial.approx = approxfun(w,Vectorize(f.norm.partial)(w))
  }

  n = length(p)
  l = rep(NA,n)
  for (i in seq(n)) {
    difference = function(l) (f.norm.partial.approx(l)-p[i]*f.norm)^2
    l[i] = stats::optimize(difference,c(0,f.max))$minimum
  }

  return(l)

}

.contourlevel.vector = function(x, p) {

  x.sort = sort(x,decreasing=TRUE)
  x.cc = cumsum(x.sort)
  norm = x.cc[length(x.cc)]
  level = rep(NA,length(p))
  for (i in seq_along(p)) {
    index = which(x.cc>=p[i]*norm)[1]
    level[i] = x.sort[index]
  }
  return(level)

}
