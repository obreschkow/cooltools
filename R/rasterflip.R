#' Flip array to be displayed with rasterImage()
#'
#' @description Flips the array A to be displayed with rasterImage, such that the first index runs from left to right and second index runs from bottom to top, like in standard Cartesian coordinates. In this way \code{rasterImage(rasterflip(A))} has the same orientation as \code{image(A)}.
#'
#' @param A n-by-m array of a monochromatic image or n-by-m-by-k array of a color image (where k is 3 or 4)
#'
#' @author Danail Obreschkow
#'
#' @export

rasterflip = function(A) {
  if (length(dim(A))==2) {
    return(t(A[,dim(A)[2]:1]))
  } else if (length(dim(A))==3) {
    return(aperm(A[,dim(A)[2]:1,],c(2,1,3)))
  } else {
    stop('Object A not recognised.')
  }
}
