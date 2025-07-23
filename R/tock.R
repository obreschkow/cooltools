#' Stop timer
#'
#' @description Stop timer and write the computation in seconds since the last call of tick().
#'
#' @param txt custom text to be displayed.
#' @param fmt character vector of format strings. It must contain exactly one \%s as placeholder for \code{txt} and one numerical format, such as \%f or \%e, as placeholder for the time.
#'
#' @examples
#'
#' tick('Sum 10 million random numbers')
#' x = sum(runif(1e7))
#' tock()
#'
#' @author Danail Obreschkow
#'
#' @return Returns the elapsed time in seconds since calling tick().
#'
#' @seealso \code{\link{tick}} \code{\link{progress}} \code{\link{error}}
#'
#' @export

tock = function(txt='', fmt=' (%.2fs). %s\n') {
  dt = as.double(proc.time()[3]-get("tickTime", envir = .cooltools.env))
  i = as.vector(gregexec('%',fmt)[[1]])
  if (length(i)!=2) stop('tock: fmt must contain exactly two placeholders %s and %f/%e.')
  j = as.vector(gregexec('%s',fmt)[[1]])
  if (length(j)!=1) stop('tock: fmt must contain exactly one placeholder %s.')
  cat(strrep("\b \b", .cooltools.env$progress_nchar))
  if (j==i[1]) {
    cat(sprintf(fmt,txt,dt))
  } else {
    cat(sprintf(fmt,dt,txt))
  }
  assign("progress_nchar", 0, envir = .cooltools.env)
  assign("timerRunning", FALSE, envir = .cooltools.env)
  invisible(dt)
}
