#' Start timer
#'
#' @importFrom pracma tic
#'
#' @description Start timer and write a custom text into the console.
#'
#' @param txt custom text
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
#' @seealso \code{\link{tock}}
#'
#' @export

tick = function(txt='Start') {
  cat(sprintf('%s',txt))
  pracma::tic()
}
