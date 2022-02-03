#' Suppress in-routine output
#'
#' @description Runs any routine or command while supressing in-routine console output
#'
#' @param x routine to be called
#'
#' @return Returns whatever the called routine returns in invisible form.
#'
#' @author Danail Obreschkow
#'
#' @examples
#' # Test function
#' test = function(x) {
#'   cat('This routine is likes to talk a lot!\n')
#'   return(x^2)
#' }
#'
#' # Standard call call:
#' y = test(5)
#' print(y)
#'
#' # Quiet call:
#' y = quiet(test(6))
#' print(y)
#'
#' @export
#'
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
