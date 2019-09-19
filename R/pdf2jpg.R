#' Convert pdf to jpg
#'
#' @description Calls the console "convert" function to convert a pdf-file into a jpeg-image. Requires "convert" to be installed already.
#'
#' @param pdf.filename filename of pdf input file
#' @param jpg.filename filename of jpeg output file
#' @param quality quality of jpeg compression
#' @param background color of background
#' @param dim size in pixels of the jpeg-image
#' @param remove.pdf logical flag. If TRUE, the pdf file is deleted after the conversion.
#' @param verbose logical flag to turn on/off console statements.
#'
#' @author Danail Obreschkow
#'
#' @export

pdf2jpg = function(pdf.filename, jpg.filename, quality=100, background='white', dim=c(1600,1200), remove.pdf=FALSE, verbose=TRUE) {

  if (verbose) cat(sprintf('Converting pdf to jpg'))

  command = sprintf('convert -density 1600x1600 -resize %dx%d -quality %d -flatten -background %s %s %s',
                    dim[1],dim[2],quality,background,pdf.filename,jpg.filename)
  system(command)

  if (remove.pdf) {
    command = sprintf('rm %s',pdf.filename)
    system(command)
  }

  if (verbose) cat(sprintf('. Done.\n'))

}
