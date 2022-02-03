#' Discrete Fourier Transform
#'
#' @importFrom stats fft
#'
#' @description Discrete Fourier Transform (DFT) with longest modes at the center in Fourier space and normalized such that dft(dft(f),inverse)=f. This is the discretization scheme described in  Appendix D of Obreschkow et al. 2013, ApJ 762. Relies on \code{\link[stats]{fft}}.
#'
#' @param f real or complex D-dimensional array containing the values to be transformed.
#' @param inverse logical flag; if TRUE the inverse Fourier transform is performed.
#' @param shift D-vector specifying the integer shift of the coordinates in Fourier space. Set to \code{shift=rep(0,D)} to produced a DFT with the longest mode at the corner in Fourier space.
#' @param simplify logical flag; if TRUE the complex output array will be simplified to a real array, if it is real within the floating point accuracy
#'
#' @return Returns an array of the same shape as \code{f}, containing the (inverse) Fourier Transform.
#'
#' @examples
#'
#' ## DFT of a 2D normal Gaussian function
#' n = 30
#' f = array(0,c(n,n))
#' for (i in seq(n)) {
#'   for (j in seq(n)) f[i,j] = exp(-(i-6)^2/4-(j-8)^2/2-(i-6)*(j-8)/2)
#' }
#' plot(NA,xlim=c(0,2.1),ylim=c(0,1.1),asp=1,bty='n',xaxt='n',yaxt='n',xlab='',ylab='')
#' rasterImage(f,0,0,1,1,interpolate=FALSE)
#' g = dft(f)
#' img = array(hsv((pracma::angle(g)/2/pi)%%1,1,abs(g)/max(abs(g))),c(n,n))
#' rasterImage(img,1.1,0,2.1,1,interpolate=FALSE)
#' text(0.5,1,'Input function f',pos=3)
#' text(1.6,1,'DFT(f)',pos=3)
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link[stats]{fft}}
#'
#' @export

dft = function(f, inverse=FALSE, shift=-floor(dim(as.array(f))/2), simplify=TRUE) {
  if (inverse) {
    g = stats::fft(cshift(f,shift),inverse=TRUE)
  } else {
    g = cshift(stats::fft(f),-shift)/length(f)
  }
  if (simplify) {
    if (mean(abs(Im(g)))/(mean(abs(g))+.Machine$double.xmin)<1e-13) g = Re(g)
  }
  return(g)
}
