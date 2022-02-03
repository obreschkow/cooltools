#' Zoom, translate and rotate array image
#'
#' @importFrom graphics rasterImage
#'
#' @description Zoom/rotate/translate an image relative to its center for images represented as simple arrays.
#'
#' @param img n-by-m-by-3 array or n-by-m-by-4 array representing an rgb(+alpha) image.
#' @param zoom zoom factor (>0).
#' @param shift 2-vector specifying the vertical+horizontal translation in units of output pixels (i.e. after zooming).
#' @param angle rotation angle in degrees.
#' @param size 2-vector specifying the vertical+horizontal dimensions of the output image. If not given, this is taken to be identical to the input image.
#' @param col background color
#' @param filter affine transformation filter; either 'none' or 'bilinear'
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
#' transzoom(img, zoom=2) # zoom by a factor 2
#'
#' @author Danail Obreschkow
#'
#' @export

transzoom = function(img=NULL, zoom=1, shift=c(0,0), angle=0, size=NULL, col='black', filter='bilinear', file.in='', file.out='', format = 'png', show.image=TRUE) {

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

  if (is.null(size)) {
    size = dim(img)[1:2]
  } else {
    size = round(size)
  }

  # rotate image
  channels = col2rgb(col,alpha=dim(img)[3]==4)/255
  if (angle!=0) {
    if (requireNamespace("EBImage", quietly=TRUE)) {
      img = EBImage::rotate(img, angle, bg.col=channels, filter=filter)
    } else {
      stop('Package EBImage is needed in function transzoom.')
    }
  }

  # zoom and translate image
  m = rbind(c(zoom,0),c(0,zoom),(size-dim(img)[1:2]*zoom)/2+shift)
  if (requireNamespace("EBImage", quietly=TRUE)) {
    img = EBImage::affine(img, bg.col=channels, filter=filter, m=m, output.dim=size)
  } else {
    stop('Package EBImage is needed in function transzoom.')
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
