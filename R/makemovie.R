#' Produce a movie from frame-drawing function
#'
#' @importFrom grDevices png dev.off
#' @importFrom plotrix draw.circle
#' @importFrom graphics arrows
#'
#' @description Generates an MP4-movie provided a custom function that plots individual frames. The routine has been developed and tested for MacOS and it requires on a working installation of ffmpeg.
#'
#' @param frame.draw function that plots an individual frame. This function must have exactly one  argument 'x', which can be integer (e.g. a simple frame index) or real (e.g. a time).
#' @param frame.index list of frame indices 'x' to be included in the movie
#' @param output.path character specifying the directory, where the movie and temporary frames are saved
#' @param output.filename movie filename without path. This filename should end on the extension '.mp4'.
#' @param width number of pixels along the horizontal axis
#' @param height number of pixels along the vertical axis
#' @param fps number of frames per second
#' @param keep.frames logical flag specifying whether the temporary directory with the individual frame files should be kept
#' @param quiet logical flag; if true, all console outputs produced by 'ffmpeg' are suppressed
#' @param separator filename separate of the system ('/' for Mac, Linux, Unix; '\' for Windows)
#' @param ffmpeg.cmd command used to call ffmpeg form a terminal. Normally, this is just 'ffmpeg'.
#' @param ffmpeg.opt compression and formatting options used with ffmpeg
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{makeframe}}
#'
#' @examples
#'
#' ## Example: Movie of a manual clock
#'
#' # Function to draw a single clock face with two hands
#' frame = function(time) {
#'   par(mar=c(0,0,0,0))
#'   cooltools::nplot(xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),pty='s')
#'   plotrix::draw.circle(0,0,1,col='#aaaaff')
#'   radius = c(0.5,0.9)
#'   speed = 2*pi/c(720,60)
#'   lwd = c(4,2)
#'   graphics::arrows(0,0,radius*sin(speed*time),radius*cos(speed*time),lwd=lwd)
#' }
#'
#' # Produce movie
#' \dontrun{
#' makemovie(frame,seq(0,60,0.5),'~/testmovie','movie.mp4',200,200)
#' }
#'
#' @export

makemovie = function(frame.draw,frame.index,
                     output.path,output.filename,
                     width=1080,height=720,fps=60,
                     keep.frames=F, quiet=T, separator='/',
                     ffmpeg.cmd='ffmpeg',
                     ffmpeg.opt='-vcodec libx264 -crf 18 -pix_fmt yuv420p') {

  # make output path, if needed
  if (substr(output.path,nchar(output.path),nchar(output.path))!=separator) {
    output.path=paste0(output.path,separator)
  }
  call = sprintf('mkdir -p %s',output.path)
  system(call)

  # make new path for frames
  frame.path = sprintf('%s%.0f%s',output.path,
                       as.numeric(Sys.time())*1e3,separator)
  call = sprintf('mkdir -p %s',frame.path)
  system(call)

  # write frames
  for (i in seq_along(frame.index)) {
    cat(sprintf('Write frame %0.6d.\n',i))
    fn = file.path(frame.path,sprintf('frame_%0.8d.png',i))
    grDevices::png(fn,width=width,height=height)
    frame.draw(frame.index[i])
    grDevices::dev.off()
  }

  # convert into movie
  cat(sprintf('Write movie file %s\n',output.filename))
  call = sprintf('%s -y -r %d -f image2 -s %dx%d -i %sframe_%%08d.png %s %s%s %s',
                 ffmpeg.cmd,fps,width,height,linuxspaces(frame.path),ffmpeg.opt,
                 linuxspaces(output.path),linuxspaces(output.filename),
                 ifelse(quiet,'-loglevel quiet',''))
  system(call)

  # remove frames
  if (!keep.frames) {
    cat('Delete individual frames.')
    call = sprintf('rm -rf %s',frame.path)
    system(call)
  }

}
