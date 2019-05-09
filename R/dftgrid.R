#' Produce coordinates for Discrete Fourier Transform
#'
#' @description Produces the coordinates in direct space (x) and Fourier space (k), corresponding to the Discrete Fourier Transform \code{dft} of this package.
#'
#' @param N number of divisions along one dimension used in direct and Fourier space
#' @param L side-length (=period) of the data long one dimension
#'
#' @return Returns a list with the vectors x and k, the corresponding steps dx and dk, and the input values N and L.
#'
#' @author Danail Obreschkow
#'
#' @export

dftgrid = function(N,L) {
  dx = L/N
  dk = 2*pi/L
  x = seq(0,N-1)*dx
  k = seq(-floor(N/2),floor((N-1)/2))*dk
  return(list(N=N,L=L,x=x,k=k,dx=dx,dk=dk))
}
