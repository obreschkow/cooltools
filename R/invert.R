#' Invert and shift colors of an image
#'
#' @importFrom png readPNG writePNG
#' @importFrom jpeg readJPEG writeJPEG
#' @importFrom grDevices rgb2hsv hsv
#' @importFrom graphics rasterImage
#'
#' @description Invert the brightness of each color channel in an image and/or circularly shifts the hue value. Optionally, a Gaussian blur can be applied.
#'
#' @param img n-by-m-by-3 array or n-by-m-by-4 array representing an rgb(+alpha) image
#' @param invert logical flag indicating whether the channel-brightness should be inverted
#' @param colshift numerical value between 0 and 1 setting the circular shift of the hue value. If \code{invert=TRUE}, choosing \code{colshift=0.5} preserves the colors, while inverting black and white.
#' @param blur numerical value >=0 defining the standard deviation of an optional Gaussian blur.
#' @param file.in optional input filename, which can be used to load an image instead of providing it via \code{img}. This filename is ignored if \code{img} is specified.
#' @param file.out optional output filename.
#' @param format one of "png" or "jpg" specifying the file format of the input and output image.
#' @param show.image logical flag specifying whether the image is displayed in the R console.
#'
#' @return Returns an n-by-m-by-3 array or n-by-m-by-4 array of the processed image.
#'
#' @examples
#'
#' img = yinyangyong # this is an example image included in the package
#'
#' # invert brightness of all channels
#' invert(img)
#'
#' # invert brightness, but preserve hue
#' invert(img, colshift=0.5)
#'
#' @author Danail Obreschkow
#'
#' @export

invert = function(img=NULL, invert=TRUE, colshift=0, blur=0, file.in='', file.out='', format = 'png', show.image=TRUE) {

  if (is.null(img)) {

    # load image
    if (!file.exists(file.in)) stop('Input file does not exist.')
    if (format=='png') {
      img = png::readPNG(file.in)
    } else if (format=='jpg' | format=='jpeg') {
      img = jpeg::readJPEG(file.in)
    } else {
      stop('If format is specified, it must be equal to "png" or "jpg".')
    }

  } else {

    # input check image
    if (!is.array(img)) stop('img must be an array of n-by-m-by-3 (or 4) elements.')
    if (length(dim(img))!=3) stop('img must be an array of n-by-m-by-3 (or 4) elements.')
    if (dim(img)[3]<3 | dim(img)[3]>4) stop('img must be an array of n-by-m-by-3 (or 4) elements.')

  }

  # split image into rgb and alpha channels
  img.rgb = img[,,1:3]
  transparency = dim(img)[3]==4
  if (transparency) {
    img.alpha = img[,,4]
  }

  # invert rgb image and shift colours
  img.rgb = as.numeric(invert)+(1-2*as.numeric(invert))*img.rgb
  h = grDevices::rgb2hsv(as.vector(img.rgb[,,1]),as.vector(img.rgb[,,2]),as.vector(img.rgb[,,3]),1)
  x = col2rgb(grDevices::hsv((h[1,]+colshift)%%1,h[2,],h[3,]))/255
  img.rgb = array(t(x),dim(img.rgb))

  # join rgb and alpha channels
  img[,,1:3] = img.rgb
  if (transparency) img[,,4] = img.alpha

  # blur image
  if (blur>0) {
    if (requireNamespace("EBImage", quietly=TRUE)) {
      img = EBImage::gblur(img,blur)
      img[img<0] = 0
      img[img>1] = 1
    } else {
      stop('Package EBImage needed in function invert, if called with blur>0.')
    }
  }

  # write image to file
  if (nchar(file.out)>0) {
    if (format=='png') {
      png::writePNG(img,file.out)
    } else if (format=='jpg' | format=='jpeg') {
      jpeg::writeJPEG(img,file.out)
    } else {
      stop('If format is specified, it must be equal to "png" or "jpg".')
    }
  }

  # plot image
  if (show.image) {
    plot(0,0,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlim=c(0,dim(img)[2]),ylim=c(0,dim(img)[1]),xlab='',ylab='',asp=1,bty='n')
    graphics::rasterImage(img,0,0,dim(img)[2],dim(img)[1])
  }

  # return image
  invisible(img)
}
