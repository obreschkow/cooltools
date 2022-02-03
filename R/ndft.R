#' Non-uniform Discrete Fourier Transform
#'
#' @importFrom graphics points legend curve par
#' @importFrom stats fft
#'
#' @description Compute the one-dimensional Non-uniform Discrete Fourier Transform (NDFT). This is needed if the data is sampled in non-uniform intervals.
#'
#' @param f vector of real or complex function values
#' @param x vector of points in direct space, typically time or position coordinates. If \code{inverse=FALSE}, \code{x} must have the same length as \code{f}.
#' @param nu vector of frequencies in units of [1/units of x].  If \code{inverse=TRUE}, \code{nu} must have the same length as \code{f}.
#' @param inverse	logical flag; if TRUE, the inverse Fourier transform is performed.
#' @param weighing logical flag; if TRUE, irregularly spaced evaluations of \code{f} will be weighted proportionally to their bin width in \code{x} (if \code{inverse=FALSE}) or \code{nu} (if \code{inverse=FALSE}).
#' @param simplify logical flag; if TRUE, the complex output array will be simplified to a real array, if it is real within the floating point accuracy.
#'
#' @return Returns a vector of the same length as \code{x} (if \code{inverse=FALSE}) or \code{nu} (if \code{inverse=TRUE}).
#'
#' @details The one-dimensional NDFT of a vector \eqn{f=(f_1,...,f_N)} is defined as \deqn{F_j=\sum_i w_i f_i exp(-2\pi i*x_i*\nu_j)} where \eqn{w_i} are optional weights, proportional to the interval around \eqn{x_i}, only used if \code{weighing=TRUE}. Likewise, the inverse NDFT is defined as \deqn{f_i=\sum_j w_j F_j exp(+2\pi i*x_i*nu_j)} where \eqn{w_j} are optional weights, proportional to the interval around \eqn{\nu_j}. In this implementation NDFTs are computed using a brute force algorithm, scaling as \eqn{O(N*N)}, which is considerably worse than the \eqn{O(N)*log(N)} scaling of FFT algorithms. It is therefore important to pick the required frequencies wisely to minimise computing times.
#'
#' @examples
#'
#' # Define an example signal
#' nu1 = 1 # [Hz] first frequency
#' nu2 = 8 # [Hz] second frequency in the signal
#' s = function(t) sin(2*pi*nu1*t)+0.7*cos(2*pi*nu2*t+5)
#'
#' # Discretize signal
#' N = 50 # number of samples
#' t.uniform = seq(0,N-1)/N
#' t.nonuniform = t.uniform^1.3
#' s.uniform = s(t.uniform)
#' s.nonuniform = s(t.nonuniform)
#'
#' # Plot signal
#' oldpar = par(mfrow = c(1, 2))
#' curve(s,0,1,500,xaxs='i',main='Time signal',xlab='Time t',ylab='s(t)',col='grey')
#' points(t.uniform,s.uniform,pch=16,cex=0.8)
#' points(t.nonuniform,s.nonuniform,pch=4,col='blue')
#' legend('topright',c('Continuous signal','Uniform sample','Non-uniform sample'),
#'        lwd=c(1,NA,NA),pch=c(NA,16,4),col=c('grey','black','blue'),pt.cex=c(1,0.8,1))
#'
#' # Uniform and non-uniform DFT
#' nu = seq(0,N-1) # discrete frequencies
#' spectrum.uniform = stats::fft(s.uniform)
#' spectrum.nonuniform = ndft(s.nonuniform,t.nonuniform,nu)
#' spectrum.wrong = stats::fft(s.nonuniform)
#'
#' # Evaluate power
#' power.uniform = Mod(spectrum.uniform)^2
#' power.nonuniform = Mod(spectrum.nonuniform)^2
#' power.wrong = Mod(spectrum.wrong)^2
#'
#' # Plot DFT and NDFT up to Nyquist frequency
#' plot(nu,power.uniform,pch=16,cex=0.8,xlim=c(0,N/2),xaxs='i',
#'                main='Power spectrum',xlab=expression('Frequency'~nu~'[Hz]'),ylab='Power')
#' points(nu,power.nonuniform,pch=4,col='blue')
#' points(nu,power.wrong,pch=1,col='red')
#' abline(v=c(nu1,nu2),col='grey',lty=2)
#' legend('topright',c('DFT of uniform sample','NDFT of non-uniform sample',
#' 'DFT of non-uniform sample (wrong)','Input frequencies'),
#'        lwd=c(NA,NA,NA,1),lty=c(NA,NA,NA,2),pch=c(16,4,1,NA),
#'        col=c('black','blue','red','grey'),pt.cex=c(0.8,1,1,NA))
#' par(oldpar)
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link[stats]{fft}}
#'
#' @export

ndft = function(f, x=seq(0,length(f)-1)/length(f), nu=seq(0,length(f)-1),
                inverse=FALSE, weighing=TRUE, simplify=TRUE) {

  # f vector of real or complex function values
  # x vector of points in direct space, typically time or position coordinates. If \code{inverse=FALSE}, \code{x} must have the same length as \code{f}.
  # nu vector of frequencies in units of [1/units of x].  If \code{inverse=TRUE}, \code{nu} must have the same length as \code{f}.
  # inverse	logical flag; if TRUE, the inverse Fourier transform is performed.
  # weighing logical flag; if TRUE, irregularly spaced evaluations of \code{f} will be weighted proportionally to their bin width in \code{x} (if \code{inverse=FALSE}) or \code{nu} (if \code{inverse=FALSE}).
  # simplify logical flag; if TRUE, the complex output array will be simplified to a real array, if it is real within the floating point accuracy.

  N = length(f)
  if (N<=1) stop('f must be a vector with more than one elements')

  k = 2*pi*nu # angular frequencies

  if (inverse) {

    # inverse NDFT
    if (length(nu)!=N) stop('nu and f must be vectors of the same length')
    if (weighing) {
      w = c(x[2]-x[1],(x[3:N]-x[1:(N-2)])/2,x[N]-x[N-1])
      w = w/sum(w)*N
    } else {
      w = 1
    }
    g = colSums(w*f*exp(+1i*cbind(k)%*%rbind(x)))/length(f)

  } else {

    # forward NDFT
    if (length(x)!=N) stop('x and f must be vectors of the same length')
    if (weighing) {
      w = c(x[2]-x[1],(x[3:N]-x[1:(N-2)])/2,x[N]-x[N-1])
      w = w/sum(w)*N
    } else {
      w = 1
    }
    g = colSums(w*f*exp(-1i*cbind(x)%*%rbind(k)))

  }

  if (simplify) {
    if (mean(abs(Im(g)))/(mean(abs(g))+.Machine$double.xmin)<1e-13) g = Re(g)
  }

  return(g)

}
