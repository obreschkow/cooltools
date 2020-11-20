#' Stop timer
#'
#' @importFrom pracma toc
#'
#' @description Stop timer and write the computation in seconds since the last call of tick().
#'
#' @examples
#'
#' tick('Sum 10 million random numbers')
#' x = sum(runif(1e7))
#' tock()
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{tick}}
#'
#' @export

tock = function() {
  cat(sprintf(' (%.2fs).\n',as.double(pracma::toc(echo=F))))
}
