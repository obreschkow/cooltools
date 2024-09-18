#' Display a single movie frame
#'
#' @importFrom grDevices png dev.off
#' @importFrom plotrix draw.circle
#' @importFrom graphics arrows par
#' @importFrom png readPNG
#'
#' @description Displays a single movie-frame in the R-console, exactly as used in a movie generated with \code{\link{makemovie}}.
#'
#' @param frame.draw function that plots an individual frame. This function must have exactly one  argument 'x', which can be integer (e.g. a simple frame index) or real (e.g. a time).
#' @param frame.index list of frame indices 'x' to be included in the movie
#' @param width integer number of pixels along the horizontal axis
#' @param height integer number of pixels along the vertical axis
#' @param cex number defining the overall scaling of line widths, font sizes, etc.
#' @param oversampling integer specifying the oversampling factor along both dimensions. If larger than 1, frames are plotted with (width*oversampling)-by-(height*oversampling) pixels and then resized back to width-by-height. This can be used to make line objects and text move more smoothly.
#' @param pngfile optional path+filename of output file to save the image. R must have write access to this file.
#'
#' @author Danail Obreschkow
#'
#' @return Returns the displayed image as n-by-my-by 4 array, representing the 4 RGBA channels of height n and width m.
#'
#' @seealso \code{\link{makemovie}}
#'
#' @examples
#'
#' ## Example: Movie of a manual clock
#'
#' # Function to draw a single clock face with two hands
#' frame = function(time) {
#'   oldpar = graphics::par(mar=c(0,0,0,0))
#'   nplot(xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),pty='s')
#'   plotrix::draw.circle(0,0,1,col='#aaaaff')
#'   radius = c(0.5,0.9)
#'   speed = 2*pi/c(720,60)
#'   lwd = c(4,2)
#'   graphics::arrows(0,0,radius*sin(speed*time),radius*cos(speed*time),lwd=lwd)
#'   graphics::par(oldpar)
#' }
#'
#' # Produce movie
#' \dontrun{
#' makeframe(frame,15,200,200)
#' }
#'
#' @export

makeframe = function(frame.draw,frame.index,width=1080,height=720,cex=1,oversampling=1,pngfile=NULL) {

  # safe use of par()
  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))

  # handle oversampling argument
  if (oversampling<1) stop('oversampling cannot be smaller than 1.')
  if (oversampling!=round(oversampling)) stop('oversampling should be an integer')
  if (oversampling>1) {
    if (!requireNamespace("EBImage", quietly=TRUE)) {
      stop('Package EBImage is needed in function makemovie.')
    }
  }

  # make unique frame file name
  if (is.null(pngfile)) {
    fn = tempfile()
    on.exit(unlink(fn)) # deletes the frame on exit
  } else {
    fn = pngfile
  }

  # write frames
  grDevices::png(fn,width=width*oversampling,height=height*oversampling,res=0.17*sqrt(width*height)*oversampling*cex)
  frame.draw(frame.index)
  grDevices::dev.off()

  # load frame
  img = png::readPNG(fn)

  # resize frame
  if (oversampling>1) {
    img = EBImage::resize(img,height,width,antialias=TRUE)
    if (!is.null(pngfile)) png::writePNG(img, fn)
  }

  # display frame
  par(bg='grey',mar=c(0,0,0,0))
  nplot(xlim=c(0,width),ylim=c(0,height),asp=1)
  rasterImage(img,0,0,width,height)

  # return
  invisible(img)

}
