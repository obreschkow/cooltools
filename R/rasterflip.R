#' Flip array to be displayed with rasterImage()
#'
#' @param A n-by-m or n-by-m-by-k array of an image
#'
#' @description Flips the raster image A to be displayed with rasterImage, such that the first index runs
# from left to right and second index runs from bottom to top.
#'
#' @examples
#' col = transparent('purple',0.5)
#'
#' @author Danail Obreschkow
#'
#' @export transparent

rasterflip = function(A) {
  if (length(dim(A))==2) {
    return(t(A[,dim(A)[2]:1]))
  } else if (length(dim(A))==3) {
    return(aperm(A[,dim(A)[2]:1,],c(2,1,3)))
  } else {
    stop('Object A not recognised.')
  }
}
