#' Convert wavelength to RGB
#'
#' @importFrom graphics plot rasterImage
#' @importFrom pracma erf
#'
#' @description Converts a given wavelength of light to an approximate RGB color value, using black in the invisible range.
#'
#' @param wavelength wavelength value (or vector), in nanometers.
#'
#' @return Returns a color string or vector of color strings with the same number of elements as \code{wavelength}.
#'
#' @source Smoothed implementation of the original Fortran version by Dan Bruton (http://www.physics.sfasu.edu/astro/color/spectra.html) and the R-function by Michael Friendly (https://gist.github.com/friendly).
#'
#' @author Danail Obreschkow
#'
#' @examples
#' lambda = seq(300,800)
#' col = matrix(wavelength2col(lambda),nrow=1)
#' plot(NA,xlim=range(lambda),ylim=c(0,1),xaxs='i',xlab='wavelength [nm]',yaxs='i',yaxt='n',ylab='')
#' rasterImage(col,min(lambda),0,max(lambda),1)
#'
#' @export

wavelength2col = Vectorize(function(wavelength){

  attenuation = 1.0
  gamma=0.9

  if (wavelength >= 380 & wavelength <= 440) {
    attenuation = ((wavelength - 380) / (440 - 380))
    attenuation = sin(attenuation*pi/2)
    R = (-(wavelength - 440) / (440 - 380))
    G = 0.0
    B = -(wavelength - 510) / (510 - 490)
  }
  else if (wavelength >= 440 & wavelength <= 490) {
    R = 0.0
    G = (wavelength - 440) / (490 - 440)
    B = -(wavelength - 510) / (510 - 490)
  }
  else if (wavelength >= 490 & wavelength <= 510) {
    R = 0.0
    G = (wavelength - 440) / (490 - 440)
    B = -(wavelength - 510) / (510 - 490)
  }
  else if (wavelength >= 510 & wavelength <= 580) {
    R = (wavelength - 510) / (580 - 510)
    G = min((wavelength - 440) / (490 - 440),(645-wavelength)/(645-580))
    B = -(wavelength - 510) / (510 - 490)
  }
  else if (wavelength >= 580 & wavelength <= 645) {
    R = 1.0
    G = (645-wavelength)/(645-580)
    B = -(wavelength - 510) / (510 - 490)
  }
  else if (wavelength >= 645 & wavelength <= 750) {
    attenuation = (750 - wavelength) / (750 - 645)
    attenuation = sin(attenuation*pi/2)
    R = 1.0
    G = 0.0
    B = -(wavelength - 510) / (510 - 490)
  }
  else {
    attenuation = 0
    R = 0.0
    G = 0.0
    B = -(wavelength - 510) / (510 - 490)
  }

  B = erf(B-0.5)*0.5+0.5
  G = erf((G-0.5)*1.5)*0.5+0.5

  # attenuate towards visibility limits and apply gamma correction
  R = (R*attenuation)^gamma
  G = (G*attenuation)^gamma
  B = (B*attenuation)^gamma

  return (rgb(R,G,B))
})
