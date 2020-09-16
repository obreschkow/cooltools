#' Turn a 64-bit integer into a unique double value
#'
#' @importFrom bit64 as.integer64
#'
#' @description Turns a 64-bit integers into unique doubles for faster comparison. The output double values are completely different from the input values.
#'
#' @param int64 input value (normally used with 64-bit integers, but also works with other types)
#'
#' @return Returns a double floating point value.
#'
#' @examples
#'
#' # The comparison of in-built types is very fast:
#' int32 = as.integer(0) # (same as int32 = 0)
#' system.time(for(i in seq(1e4)) comparison=int32==int32)
#'
#' # The comparison of 64-bit integers is very slow:
#' int64 = bit64::as.integer64(0)
#' system.time(for(i in seq(1e4)) comparison=int64==int64)
#'
#' # The comparison of converted 64-bit integers is again fast:
#' int64d = uniquedouble(int64)
#' system.time(for(i in seq(1e4)) comparison=int64d==int64d)
#'
#' @author Danail Obreschkow
#'
#' @export

uniquedouble = function(int64) {
  return(sin(as.double(int64%%2147483647))+as.double(int64/2147483647))
}
