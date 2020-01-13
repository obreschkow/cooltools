#' Build self-similar merger tree
#'
#' @description Builds a self-similar rooted directional tree (e.g. a halo merger tree) to be displayed with \code{\link{plottree}}.
#'
#' @param m vector of relative masses (normalization is irrelevant)
#' @param nlevels number of progenitor levels
#'
#' @examples
#'
#' ## build and show a self-similar binary tree with mass ratio 2:1
#' tree = buildtree(m=c(2,1), nlevels=4)
#' plottree(tree)
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{plottree}}
#'
#' @export

buildtree = function(m=c(1,1), nlevels=2) {

  if (!is.vector(m)) stop('m must be a vector.')
  if (nlevels<=0) stop('nlevels must be a positive integer.')

  x = m/sum(m)
  mass = c(1)
  descendant = c(0)
  nmin = 1

  for (level in seq(nlevels)) {

    n = length(mass)
    for (i in seq(nmin,n)) {
      mass = c(mass,mass[i]*x)
      descendant = c(descendant,rep(i,length(x)))
    }
    nmin = n+1

  }

  return(list(mass = mass, descendant = descendant))

}
