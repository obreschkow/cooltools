#' Fast cosmology conversion functions
#'
#' @importFrom celestial cosdist
#' @importFrom stats splinefun
#'
#' @description Generates all 20 conversion functions between redshift (z), luminosity distance (dl), comoving distance (dc) and angular diameter distance (da), and lookback time (t = light travel time from specified redshift); based on the *celestial* package.
#'
#' @param zmin minimum redshift for which the conversion functions are used
#' @param zmax maximum redshift for which the conversion functions are used
#' @param dz redshift interval on which the conversion functions are interpolated (default of 0.02 is normally largely sufficient)
#' @param H0 local Hubble constant in units of km/s/Mpc (default 70).
#' @param OmegaM local normalised matter density (default 0.3).
#' @param ... other cosmological parameters accepted by \code{cosdist} of the *celestial* package. Defaults are OmegaL=1-OmegaM-OmegaR, OmegaR=0, w0=-1, wprime=0.
#'
#' @return Returns a list of 20 vectorized functions; e.g. dc2z to convert from comoving distance to redshift. Also contains the \code{age} of the universe at z=0. All distances are in units of Mpc and times are in units of Gyr.
#'
#' @author Danail Obreschkow (based on *celestial* package by Aaron Robotham)
#'
#' @examples
#' ## uses a flat LCDM cosmology with h=0.68, OmegaM=0.32 and OmegaL=0.68
#' cosmo = cosmofct(0,1,H0=68,OmegaM=0.32)
#' curve(cosmo$z2dl(x),0,1,xlab='z',ylab='distance',col='red')
#' curve(cosmo$z2dc(x),0,1,col='black',add=TRUE)
#' curve(cosmo$z2da(x),0,1,col='blue',add=TRUE)
#' d = seq(500,5000,500)
#' points(cosmo$dl2z(d),d,pch=16,col='red')
#' points(cosmo$dc2z(d),d,pch=16,col='black')
#' points(cosmo$da2z(d),d,pch=16,col='blue')
#'
#' @export

cosmofct = function(zmin=0,zmax=1,dz=0.02,H0=70,OmegaM=0.3,...) {

  z = seq(zmin,zmax,length.out=ceiling((zmax-zmin)/dz)+1)
  out = quiet(celestial::cosdist(z,age=TRUE,H0=H0,OmegaM=OmegaM,...))
  dl = out$LumDist
  dc = out$CoDist
  da = out$AngDist
  t = out$TravelTime

  return(list(z2dl = stats::splinefun(z,dl), z2dc = stats::splinefun(z,dc), z2da = stats::splinefun(z,da), z2t = stats::splinefun(z,t),
              dl2z = stats::splinefun(dl,z), dl2dc = stats::splinefun(dl,dc), dl2da = stats::splinefun(dl,da), dl2t = stats::splinefun(dl,t),
              dc2z = stats::splinefun(dc,z), dc2dl = stats::splinefun(dc,dl), dc2da = stats::splinefun(dc,da), dc2t = stats::splinefun(dc,t),
              da2z = stats::splinefun(da,z), da2dl = stats::splinefun(da,dl), da2dc = stats::splinefun(da,dc), da2t = stats::splinefun(da,t),
              t2z = stats::splinefun(t,z), t2dl = stats::splinefun(t,dl), t2dc = stats::splinefun(t,dc), t2da = stats::splinefun(t,da),
              age = out$UniAgeNow[1]))

}
