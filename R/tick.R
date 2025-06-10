#' Start timer
#'
#' @description Start timer and write a custom text into the console.
#'
#' @param txt custom text to be displayed.
#'
#' @examples
#'
#' tick('Sum 10 million random numbers')
#' x = sum(runif(1e7))
#' tock()
#'
#' @author Danail Obreschkow
#'
#' @return None
#'
#' @seealso \code{\link{tock}} \code{\link{progress}}
#'
#' @export

tick = function(txt='Start') {
  cat(sprintf('%s',txt))
  assign("tickTime", proc.time()[3], envir = .cooltools.env)
  assign("progress_nchar", 0, envir = .cooltools.env)
  assign("timerRunning", TRUE, envir = .cooltools.env)
}
