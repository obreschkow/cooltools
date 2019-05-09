#' Flip array to be displayed with rasterImage()
#'
#' @param A n-by-m or n-by-m-by-k array of an image (where k is 3 or 4)
#'
#' @description Flips the array A to be displayed with rasterImage, such that the first index runs
# from left to right and second index runs from bottom to top. In this way \code{rasterImage(rasterflip(A))} has the same orientation as \code{image(A)}.
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
