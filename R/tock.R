#' Stop timer
#'
#' @importFrom pracma toc
#'
#' @description Stop timer and write the computation in seconds since the last call of tick().
#'
#' @param txt optional custom text to be displayed
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
#' @seealso \code{\link{tick}}
#'
#' @export

tock = function(txt='') {
  cat(sprintf(' (%.2fs). %s\n',as.double(pracma::toc(echo=F)),txt))
}
