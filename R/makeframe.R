#' Display a single movie frame
#'
#' @importFrom grDevices png dev.off
#' @importFrom plotrix draw.circle
#' @importFrom graphics arrows
#' @importFrom png readPNG
#'
#' @description Displays a single movie-frame in the R-console, exactly as used in a movie generated with \code{\link{makemovie}}.
#'
#' @param frame.draw function that plots an individual frame. This function must have exactly one  argument 'x', which can be integer (e.g. a simple frame index) or real (e.g. a time).
#' @param frame.index list of frame indices 'x' to be included in the movie
#' @param width number of pixels along the horizontal axis
#' @param height number of pixels along the vertical axis
#' @param keep.frame logical flag specifying whether to keep the temporary png-file of the frame
#'
#' @author Danail Obreschkow
#'
#' @return None
#'
#' @seealso \code{\link{makemovie}}
#'
#' @examples
#'
#' ## Example: Movie of a manual clock
#'
#' # Function to draw a single clock face with two hands
#' frame = function(time) {
#'   par(mar=c(0,0,0,0))
#'   nplot(xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),pty='s')
#'   plotrix::draw.circle(0,0,1,col='#aaaaff')
#'   radius = c(0.5,0.9)
#'   speed = 2*pi/c(720,60)
#'   lwd = c(4,2)
#'   graphics::arrows(0,0,radius*sin(speed*time),radius*cos(speed*time),lwd=lwd)
#' }
#'
#' # Produce movie
#' \dontrun{
#' makeframe(frame,15,200,200)
#' }
#'
#' @export

makeframe = function(frame.draw,frame.index,width=1080,height=720,keep.frame=FALSE) {

  # make unique frame name
  fn = sprintf('frame_tmp_%.0f.png',as.numeric(Sys.time())*1e3)

  # write frames
  grDevices::png(fn,width=width,height=height)
  frame.draw(frame.index)
  grDevices::dev.off()

  # display frame
  img = png::readPNG(fn)
  nplot(xlim=c(0,width),ylim=c(0,height),asp=1)
  rasterImage(img,0,0,width,height)

  # remove frame
  if (!keep.frame) {
    call = sprintf('rm -r %s',fn)
    system(call)
  }

}

