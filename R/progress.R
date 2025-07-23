#' Show progress while timer in running
#'
#' @description Show progress while a timer is running following a call of tick().
#'
#' @param txt custom text to be displayed. If not provided, the time since calling tick() is displayed.
#'
#' @examples
#'
#' tick('Test')
#' Sys.sleep(.1)
#' for (i in seq(3)) {
#'   progress(i)
#'   Sys.sleep(.1)
#' }
#' tock()
#'
#' @author Danail Obreschkow
#'
#' @return None
#'
#' @seealso \code{\link{tick}} \code{\link{tock}} \code{\link{error}}
#'
#' @export

progress = function(txt=NULL) {

  if (is.null(.cooltools.env$timerRunning) || !.cooltools.env$timerRunning) {
    cat('Warning: progress can only be called if a timer has been started with tick(), but not yet stopped with tock().')
  } else {
    if (is.null(txt)) txt=sprintf('(%.2fs)',as.double(proc.time()[3]-get("tickTime", envir = .cooltools.env)))
    cat(sprintf(' %s%s',strrep("\b \b", .cooltools.env$progress_nchar),txt))
    assign("progress_nchar", nchar(txt)+1, envir = .cooltools.env)
  }

}
