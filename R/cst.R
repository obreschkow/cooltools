#' Scientific constants
#'
#' @description The list \code{cst} contains useful scientific constants in SI units, mainly for astrophysics.
#'
#' @examples
#' # print all the constants to console
#' for (i in seq(length(cst))) cat(sprintf('%6s = %.12e\n',names(cst)[i],cst[i]))
#'
#' @author Danail Obreschkow
#'
#' @return None
#'
#' @export

cst = list(G = 6.67408e-11, # [m^3/kg/s^2] gravitational constant
           kb = 1.38064852e-23, # [m^2 kg s^(-2) K^(-1)] Boltzmann constant
           c = 299792458, # [m/s] speed of light
           h = 6.62607015e-34, # [J*s] Planck constant
           e = 1.602176634e-19, # [A*s] electron charge
           Na = 6.02214076, # [-] Avogadro constant
           hbar = 6.62607015e-34/2/pi, # [J*s] Planck constant
           Msun = 1.98847e30, # [kg] solar mass
           Mearth = 5.972e24, # [kg] Earth mass
           dalton = 1.66053906661e-27, # [kg] atomic unit mass (=1/12 of the mass of an unbound neutral atom of carbon-12 in its nuclear and electronic ground state and at rest)
           AU = 149597870700, # [m] astronomical unit
           yr = 31557600, # [s] Julian astronomical year year
           month = 2629743.83333, # [s] month = year/12
           day = 86400, # [s] day
           hour = 3600, # [s] hour
           min = 60, # [s] minute
           pc = 3.0857e16, # [m] parsec
           kpc = 3.0857e19, # [m] kiloparsec
           Mpc = 3.0857e22, # [m] Megaparsec
           Gpc = 3.0857e25, # [m] Gigaparsec
           ly = 9.4605284, # [m] light-year
           fHI = 1420405751.768) # [Hz] rest-frame frequency of hydrogen 21cm line
