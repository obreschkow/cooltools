#' Numerical equality check
#'
#' @description Checks if two or more numerical values are identical within a relative numerical tolerance
#'
#' @param x vector or array of values to compare
#' @param ... optional additional values to compare
#' @param stoptext optional character string; if given, the routine stops if the values are not all equal and adds the character string to the error message
#' @param eps relative numerical tolerance
#'
#' @return Returns a logical value. TRUE means that all values of \code{x} are equal within the specified relative tolerance; also returns TRUE if all values are Inf or NA or NaN.
#'
#' @author Danail Obreschkow
#'
#' @examples
#' # almost identical values
#' x = c(acos(1/sqrt(2)),pi/4)
#' print(x)
#' print(x[1]==x[2])
#' print(is.equal(x))
#'
#' # various other examples
#' print(is.equal(1,2,3))
#' print(is.equal(1,NA,3))
#' print(is.equal(Inf,NA))
#' print(is.equal(NaN,NA))
#' print(is.equal(NaN,Inf))
#' print(is.equal(Inf,Inf,Inf))
#' print(is.equal(NA,NA))
#' print(is.equal(NaN,NaN))
#' print(is.equal(1.4,1.4))
#' print(is.equal(1.4,1.400000001))
#' print(is.equal(1.4,1.400000001,1.41))
#' print(is.equal(0,0,0,0))
#'
#' @export

is.equal = function(x, ..., stoptext=NULL, eps=sqrt(.Machine$double.eps)) {

  x = c(as.vector(x),as.vector(unlist(list(...))))
  if (length(x)<=1) stop('is.equal requires at least 2 input values')

  if (length(unique(is.finite(x)))>1) {

    out = FALSE

  } else if (all(is.na(x)&!is.nan(x)) | all(is.nan(x)) | all(is.infinite(x))) {

    out = TRUE

  } else if (any(!is.finite(x))) {

    out = FALSE

  } else {

    # at this stage x should only be made of finite numerical values
    if (all(x==0)) {
      out = TRUE
    } else if (any(x==0)) {
      out = FALSE
    } else {
      out = max(abs(x/cshift(x,1)-1))<eps
    }

  }

  if (!is.null(stoptext) & !out) stop(sprintf('non-equal values, %s',stoptext))

  return(out)

}
