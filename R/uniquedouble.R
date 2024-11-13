#' Map positive 64-bit integers onto unique doubles
#'
#' @importFrom bit64 is.integer64 as.integer64 lim.integer64
#'
#' @description Uses a reversible one-to-one mapping to converts non-negative 64-bit integers, produced via the \code{bit64} package, to double-precision floating-point values for faster handling of 64-bit integer indices, e.g. comparison of indices, count number of unique indices, etc. The double values are completely different from the corresponding integer values.
#'
#' @param x If inverse=\code{FALSE} (default), this is a scalar/vector/array of non-negative integers, typically 64-bit integers. If inverse=\code{TRUE}, it is a scalar/vector/array of doubles.
#' @param inverse Logical flag. If \code{TRUE}, doubles are converted back to their original 64-bit integers.
#'
#' @return Scalar/vector/array of double values, if inverse=\code{FALSE}, or integer64 values, if inverse=\code{TRUE}.
#'
#' @examples
#' # produce example vector of non-negative 64-bit integers
#' n = 1e3
#' x = c(bit64::as.integer64(0:n),
#'       bit64::as.integer64('9218868437227405311')+c(-n:n),
#'       bit64::lim.integer64()[2]-c(0:n))
#'
#' # map this vector onto unique doubles
#' y = uniquedouble(x)
#'
#' # check if the double-vector can be inverted back to the integer-vector
#' cat(sprintf('Check inversion = %s.\n', all(x==uniquedouble(y, inverse=TRUE))))
#'
#' # measure the time taken to compare all the int64 values
#' tick('Compare original integer64 values.')
#' comparison = TRUE
#' for(i in seq_along(x)) comparison = comparison & x[i]==x[i]
#' tock(comparison, fmt=" Time taken = %.3fs. Test result = %s.\n")
#'
#' # measure the time taken to compare all the corresponding double values
#' tick('Compare corresponding double values.')
#' comparison = TRUE
#' for(i in seq_along(y)) comparison = comparison & y[i]==y[i]
#' tock(comparison, fmt=" Time taken = %.3fs. Test result = %s.\n")
#'
#' @author Danail Obreschkow
#'
#' @export

uniquedouble = function(x, inverse=FALSE) {
  max = bit64::as.integer64('9218868437227405311') # max int64 with a finite value in double
  if (inverse) {
    if (!is.double(x)) x = as.double(x)
    class(x) = 'integer64'
    x[x<0] = -x[x<0]
  } else {
    if (!is.integer64(x)) x = bit64::as.integer64(x)
    x[x>max] = -x[x>max]
    class(x) = 'numeric'
  }
  return(x)
}
