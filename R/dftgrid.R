#' Produce coordinates for Discrete Fourier Transform
#'
#' @description Produces the direct space coordinates (x) and Fourier space frequencies (f) and angular frequencies (k), corresponding to the Discrete Fourier Transform \code{dft} of this package.
#'
#' @param N number of divisions along one dimension used in direct and Fourier space
#' @param L side-length (=period) of the data long one dimension
#' @param x0 zero-point of x-coordinates (=0 in most applications)
#' @param k0 zero-point of k-coordinates (=0 or -floor(N/2)*dk in most applications)
#'
#' @return Returns a list with:
#' \item{x}{vector of direct space coordinates}
#' \item{f}{vector of Fourier space coordinates (frequencies)}
#' \item{k}{vector of Fourier space coordinates (angular frequencies)}
#' \item{dx}{spacing of x-values}
#' \item{df}{spacing of f-values}
#' \item{dk}{spacing of k-values}
#' \item{L}{input value of L}
#' \item{N}{input value of N}
#'
#' @author Danail Obreschkow
#'
#' @export

dftgrid = function(N, L, x0=0, k0=-floor(N/2)*2*pi/L) {
  dx = L/N
  df = 1/L
  dk = 2*pi/L
  x = x0+seq(0,N-1)*dx
  k = k0+seq(0,N-1)*dk
  f = k0/(2*pi)+seq(0,N-1)*df
  return(list(x=x,f=f,k=k,dx=dx,df=df,dk=dk,N=N,L=L))
}
