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
#' @param oversampling integer specifying the oversampling factor along both dimensions. If larger than 1, frames are plotted with width*overampling-by-height*oversampling pixels and then resized back to width-by-height. This can be used to make line objects and text move more smoothly. Importantly, line widths and text sizes have to be scaled by the same oversampling factor inside the provided frame.draw argument.
#' @param fps number of frames per second
#' @param keep.frames logical flag specifying whether the temporary directory with the individual frame files should be kept. If \code{manual} is set to \code{TRUE}, the frames are always kept.
#' @param quiet logical flag; if true, all console outputs produced by 'ffmpeg' are suppressed
#' @param separator filename separate of the system ('/' for Mac, Linux, Unix; '\' for Windows)
#' @param ffmpeg.cmd command used to call ffmpeg form a terminal. Normally, this is just 'ffmpeg'.
#' @param ffmpeg.opt compression and formatting options used with ffmpeg
#' @param manual logical flag; if true, ffmpeg is not called from within the code and the frames are never deleted. The suggested linux command line is returned as output.
#' @param first.index integer specifying the first index of the vector frame.index to consider. Choosing a value larger than the default (1) can be used to continue a previously interrupted call of makemovie and/or to call makemovie from different R sessions in parallel.
#' @param last.index integer specifying the last index of the vector frame.index to consider. Choosing a value smaller than the default (length(frame.index)) can be used to continue a previously interrupted call of makemovie and/or to call makemovie from different R sessions in parallel.
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
                     width=1080,height=720,fps=60,
                     keep.frames=FALSE, quiet=FALSE, separator='/',
                     ffmpeg.cmd='ffmpeg',
                     ffmpeg.opt='-vcodec libx264 -crf 18 -pix_fmt yuv420p',
                     manual=FALSE,
                     oversampling=1,
                     first.index=1,
                     last.index=length(frame.index)) {

  if (oversampling<1) stop('oversampling cannot be smaller than 1.')
  if (oversampling!=round(oversampling)) stop('oversampling should be an integer')
  if (oversampling>1) {
    if (!requireNamespace("EBImage", quietly=TRUE)) {
      stop('Package EBImage is needed in function makemovie.')
    }
  }

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
  nframes = length(frame.index)
  dt = rep(NA,nframes)
  t = k = 0
  for (i in seq(first.index,last.index)) {
    cat(sprintf('Write frame %d/%d.',i,nframes))
    pracma::tic()
    fn = file.path(frame.path,sprintf('frame_%0.8d.png',i))
    grDevices::png(fn,width=width*oversampling,height=height*oversampling)
    frame.draw(frame.index[i])
    grDevices::dev.off()
    if (oversampling>1) {
      img = png::readPNG(fn)
      img = EBImage::resize(img,height,width,antialias=TRUE)
      png::writePNG(img, fn)
    }
    dt[i] = as.double(pracma::toc(echo = F))
    t = t+dt[i]
    k = k+1
    cat(sprintf(' (%0.3fs, FPS=%0.1f)\n',dt[i],k/t))
  }

  # convert into movie
  .linuxspaces = function(txt) gsub(' ','\\\\ ',txt)
  call = sprintf('%s -y -r %d -f image2 -s %dx%d -i %sframe_%%08d.png %s %s%s %s',
                 ffmpeg.cmd,fps,width,height,.linuxspaces(frame.path),ffmpeg.opt,
                 .linuxspaces(output.path),.linuxspaces(output.filename),
                 ifelse(quiet,'-loglevel quiet',''))
  if (manual) {
    cat('To make movie from frames, call:\n')
    cat(sprintf('%s\n',call))
  } else {
    cat(sprintf('Write movie file %s\n',output.filename))
    system(call)
  }

  # remove frames
  if (!keep.frames & !manual) {
    cat('Delete individual frames.')
    delcall = sprintf('rm -rf %s',frame.path)
    system(delcall)
  }

  invisible(call)

}
