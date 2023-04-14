#' Round a vector of floating-point values while preserving their sum
#'
#' @description Rounds the values in a vector up and down, preserving the sum of the vector and minimizing the total rounding error under this condition. An example where this is useful is when rounding a vector of percentages, where the total should add up to 100 percent.
#'
#' @param x vector of values
#' @param digits optional non-negative integer specifying the number of digits of the rounded numbers
#'
#' @return Returns a vector of rounded values of the same length as \code{x}.
#'
#' @examples
#' x = runif(5)
#' x = x/sum(x)*100
#' print(x)
#' print(sum(x))
#' y = smartround(x)
#' print(y)
#' print(sum(y))
#' y2 = smartround(x,2)
#' print(y2)
#' print(sum(y2))
#'
#' @author Danail Obreschkow
#'
#' @export

smartround = function(x, digits = 0) {
  up = 10^digits
  x = x*up
  y = floor(x)
  indices = tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  return(y/up)
}
