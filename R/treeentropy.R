#' Tree entropy of halo or galaxy mergers
#'
#' @description Computes the tree entropy and generalized entropy of a merger of n>=0 masses, accounting also for smooth accretion. See Obreschkow et al. 2020 (arxiv.org/abs/1911.11959) for the astrophysical context and mathematical definitions.
#'
#' @param m vector of masses of the merging haloes (normalization is irrelevant)
#' @param s vector of tree entropies of the merging haloes
#' @param mfinal final mass
#' @param alpha free parameter specifying the relative importance of different orders of mergers
#' @param beta free parameter specifying the importance of the most destructive mergers
#' @param gamma free parameter specifying the importance of smooth accretion
#' @param generalized logical flag, if set to \code{TRUE} the function returns the generalized entropy rather than the tree entropy
#'
#' @examples
#'
#' ## check major merger
#' txt = 'Tree entropy of an equal-mass binary merger with zero input entropy'
#' cat(sprintf('%s = %.5f = beta\n',txt,tree.entropy()))
#'
#' ## check smooth accretion
#' txt = 'Tree entropy of a maximum-entropy halo that doubles its mass by smooth accretion'
#' cat(sprintf('%s = %.5f = (1/2)^gamma\n',txt,tree.entropy(1,1,2)))
#'
#' ## check nearly smooth collapse
#' txt = 'Tree entropy of a halo grown from an instantaneous merger of 1e6 haloes of random entropies'
#' cat(sprintf('%s = %.5f ~ 0\n',txt,tree.entropy(rep(1,1e6),runif(1e6))))
#'
#' @author Danail Obreschkow
#'
#' @export

tree.entropy <- function(m=c(1,1), s=m*0, mfinal=sum(m),
                         alpha = 1/log(2)+1, beta=3/4, gamma=1/3,
                         generalized=FALSE) {

  # derived parameters
  f = (alpha-1)*exp(1)
  a = (2-gamma)/f
  b = exp(1/(alpha-1))*(1-beta)-1-a

  # normalized masses
  e = 1e-20
  x = m/(sum(m)+e)

  # generalized entropy
  H = -f*sum(x^alpha*log(x+e)) # generalized entropy

  if (generalized) {

    return(H)

  } else {

    # compute new entropy from merger
    w = 1+a*H+b*H^2
    snew = H+w*sum(x^2*(s-H))

    # smooth accretion (gains and losses)
    snew = min(1.0,snew*(sum(m)/mfinal)^gamma)

    return(snew)

  }
}
