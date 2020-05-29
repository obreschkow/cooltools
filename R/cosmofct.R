#' Fast cosmology conversion functions
#'
#' @importFrom celestial cosdist
#' @importFrom stats splinefun
#'
#' @description Generates all 16 conversion functions between redshift (z), luminosity distance (dl), comoving distance (dc) and angular diameter distance (da), based on the *celestial* package.
#'
#' @param zmin minimum redshift for which the conversion functions are used
#' @param zmax maximum redshift for which the conversion functions are used
#' @param dz redshift interval on which the conversion functions are interpolated
#' @param ... cosmological parameters accepted by \code{cosdist} of the *celestial* package. Defaults are H0=100, OmegaM=0.3, OmegaL=1-OmegaM-OmegaR, OmegaR=0, w0=-1, wprime=0.
#'
#' @return Returns a list of 16 vectorized functions; e.g. dc2z to convert from comoving distance to redshift.
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

cosmofct = function(zmin=0,zmax=1,dz=0.01,...) {

  z = seq(zmin,zmax,length.out=ceiling((zmax-zmin)/dz)+1)
  dl = cosdist(z,...)$LumDist
  dc = dl/(1+z)
  da = dl/(1+z)^2
  return(list(z2dl = splinefun(z,dl), z2dc = splinefun(z,dc), z2da = splinefun(z,da),
              dl2z = splinefun(dl,z), dl2dc = splinefun(dl,dc), dl2da = splinefun(dl,da),
              dc2z = splinefun(dc,z), dc2dl = splinefun(dc,dl), dc2da = splinefun(dc,da),
              da2z = splinefun(da,z), da2dl = splinefun(da,dl), da2dc = splinefun(da,dc)))

}
