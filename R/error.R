#' Stop code and produce an error message
#'
#' @description Stops the code and produces an error message, like the stop() function of the base library, but terminating a timer started with \code{\link{tick}}, if running.
#'
#' @param ... zero or more objects which can be coerced to character (and which are pasted together with no separator) or a single condition object.
#'
#' @author Danail Obreschkow
#'
#' @return None. geterrmessage() returns the last error message, as a character string.
#'
#' @seealso \code{\link{tick}} \code{\link{tock}} \code{\link{progress}}
#'
#' @export

error = function(...) {

  if (!is.null(.cooltools.env$timerRunning) && .cooltools.env$timerRunning) cat(' Interrupted.\n')

  base::stop(..., call.=FALSE)

}
