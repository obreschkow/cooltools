#' Associated Legendre Polynomials
#'
#' @description Compute associated Legendre polynomials P_l^m(x), defined as the canonical solutions of the general Legendre equation. These polynomials are used, for instance, to compute the spherical harmonics.
#'
#' @param x real argument between -1 and +1, can be a vector
#' @param l degree of the polynomial (0,1,2,3,4,...)
#' @param m order of the polynomial (-l,-l+1,...,l); for negative values the standard convention is used: if m>0, then P(x,l,-m) = P(x,l,m) (-1)^m*factorial(l-m)/factorial(l+m).
#'
#' @return Returns a vector with the same number of elements as \code{x}
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{sphericalharmonics}}
#'
#' @export

alp = Vectorize(function(x, l=0, m=0) {
  if (m<0) {
    m = -m
    f = (-1)^m*factorial(l-m)/factorial(l+m)
  } else {
    f = 1
  }
  k = seq(m,l)
  f*(-1)^m*2^l*(1-x^2)^(m/2)*sum(factorial(k)/factorial(k-m)*x^(k-m)*choose(l,k)*choose((l+k-1)/2,l))
})
