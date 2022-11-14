#' Stop timer
#'
#' @importFrom pracma toc
#'
#' @description Stop timer and write the computation in seconds since the last call of tick().
#'
#' @param txt custom text to be displayed
#' @param fmt character vector of format strings. It must contain exactly one \%s as placeholder for \code{txt} and one numerical format, such as \%f or \%e, as placeholder for the time
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
#' @seealso \code{\link{tick}}
#'
#' @export

tock = function(txt='', fmt=' (%.2fs). %s\n') {
  dt = as.double(pracma::toc(echo=F))
  i = as.vector(gregexec('%',fmt)[[1]])
  if (length(i)!=2) stop('tock: fmt must contain exactly two placeholders %s and %f/%e.')
  j = as.vector(gregexec('%s',fmt)[[1]])
  if (length(j)!=1) stop('tock: fmt must contain exactly one placeholder %s.')
  if (j==i[1]) {
    cat(sprintf(fmt,txt,dt))
  } else {
    cat(sprintf(fmt,dt,txt))
  }
  invisible(dt)
}
