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
#' @param ... cosmological parameters accepted by \code{cosdist} of the *celestial* package. Defaults are H0=100, OmegaM=0.3, OmegaL=1-OmegaM-OmegaR, OmegaR=0, w0=-1, wprime=0.
#'
#' @return Returns a list of 20 vectorized functions; e.g. dc2z to convert from comoving distance to redshift. Also lists the \code{age} of the universe at z=0. All distances are in units of Mpc and times are in units of Gyr.
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

cosmofct = function(zmin=0,zmax=1,dz=0.02,...) {

  z = seq(zmin,zmax,length.out=ceiling((zmax-zmin)/dz)+1)
  out = quiet(celestial::cosdist(z,age=TRUE,...))
  dl = out$LumDist
  dc = out$CoDist
  da = out$AngDist
  t = out$TravelTime

  return(list(z2dl = splinefun(z,dl), z2dc = splinefun(z,dc), z2da = splinefun(z,da), z2t = splinefun(z,t),
              dl2z = splinefun(dl,z), dl2dc = splinefun(dl,dc), dl2da = splinefun(dl,da), dl2t = splinefun(dl,t),
              dc2z = splinefun(dc,z), dc2dl = splinefun(dc,dl), dc2da = splinefun(dc,da), dc2t = splinefun(dc,t),
              da2z = splinefun(da,z), da2dl = splinefun(da,dl), da2dc = splinefun(da,dc), da2t = splinefun(da,t),
              t2z = splinefun(t,z), t2dl = splinefun(t,dl), t2dc = splinefun(t,dc), t2da = splinefun(t,da),
              age = out$UniAgeNow[1]))

}
