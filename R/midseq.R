#' Mid-points of regular grid
#'
#' @description Compute the mid-point positions of a one-dimensional regular grid of n equal intervals.
#'
#' @param min left boundary of first bin
#' @param max right boundary of last bin
#' @param n number of bins
#'
#' @return vector of mid points
#'
#' @author Danail Obreschkow
#'
#' @export

midseq = function(min, max, n=1) {
  if (length(min)!=1) stop('mid must be a single value')
  if (length(max)!=1) stop('max must be a single value')
  if (length(n)!=1) stop('n must be a single value')
  (seq(n)-0.5)/n*(max-min)+min
}
