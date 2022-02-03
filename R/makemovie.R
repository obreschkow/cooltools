#' Produce a movie from frame-drawing function
#'
#' @importFrom grDevices png dev.off
#' @importFrom plotrix draw.circle
#' @importFrom graphics arrows
#' @importFrom png readPNG writePNG
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
#' @param smear optional number of sub-frames used to smear each frame. If given, the function frame.draw must accept continuous arguments in between the values of frame.index.
#' @param keep.frames logical flag specifying whether the temporary directory with the individual frame files should be kept
#' @param quiet logical flag; if true, all console outputs produced by 'ffmpeg' are suppressed
#' @param separator filename separate of the system ('/' for Mac, Linux, Unix; '\' for Windows)
#' @param ffmpeg.cmd command used to call ffmpeg form a terminal. Normally, this is just 'ffmpeg'.
#' @param ffmpeg.opt compression and formatting options used with ffmpeg
#' @param manual logical flag; if true, ffmpeg is not called from within the code and the frames are never deleted. The suggested linux command line is returned as output.
#'
#' @return Linux command line to convert frames into movie using ffmpeg.
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
#' makemovie(frame,seq(0,60,0.5),'~/testmovie','movie.mp4',200,200)
#' }
#'
#' @export

makemovie = function(frame.draw,frame.index,
                     output.path,output.filename,
                     width=1080,height=720,fps=60,smear=NULL,
                     keep.frames=FALSE, quiet=TRUE, separator='/',
                     ffmpeg.cmd='ffmpeg',
                     ffmpeg.opt='-vcodec libx264 -crf 18 -pix_fmt yuv420p',
                     manual=FALSE) {

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
  if (is.null(smear)) {

    for (i in seq_along(frame.index)) {
      cat(sprintf('Write frame %0.6d.\n',i))
      fn = file.path(frame.path,sprintf('frame_%0.8d.png',i))
      grDevices::png(fn,width=width,height=height)
      frame.draw(frame.index[i])
      grDevices::dev.off()
    }

  } else {

    for (i in seq_along(frame.index)) {

      cat(sprintf('Write frame %0.6d.\n',i))

      # make subframe indices and weights
      if (i==1 | i==length(frame.index)) {
        subframe.index = frame.index[i]
        weight = 1
      } else {
        subframe.index = seq(frame.index[i-1],frame.index[i+1],length=smear)
        weight = exp(-seq(-1,1,length=smear)^2)
      }

      # draw all subframes
      for (k in seq_along(subframe.index)) {
        fn = file.path(frame.path,sprintf('subframe_%0.8d.png',k))
        grDevices::png(fn,width=width,height=height)
        frame.draw(subframe.index[k])
        grDevices::dev.off()
      }

      # merge subframes
      for (k in seq_along(subframe.index)) {
        fn = file.path(frame.path,sprintf('subframe_%0.8d.png',k))
        if (k==1) {
          img = weight[k]*png::readPNG(fn)
        } else {
          img = img+weight[k]*png::readPNG(fn)
        }
      }
      img = img/sum(weight)
      fn = file.path(frame.path,sprintf('frame_%0.8d.png',i))
      png::writePNG(img, fn)
    }

  }

  # convert into movie
  .linuxspaces = function(txt) gsub(' ','\\\\ ',txt)
  cat(sprintf('Write movie file %s\n',output.filename))
  call = sprintf('%s -y -r %d -f image2 -s %dx%d -i %sframe_%%08d.png %s %s%s %s',
                 ffmpeg.cmd,fps,width,height,.linuxspaces(frame.path),ffmpeg.opt,
                 .linuxspaces(output.path),.linuxspaces(output.filename),
                 ifelse(quiet,'-loglevel quiet',''))
  if (!manual) system(call)

  # remove frames
  if (!keep.frames) {
    cat('Delete individual frames.')
    delcall = sprintf('rm -rf %s',frame.path)
    system(delcall)
  }

  invisible(call)

}
