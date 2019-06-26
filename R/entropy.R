#' Information entropy
#'
#' @description Computes the information entropy H=sum(p*log_b(p)), also known as Shannon entropy, of a probability vector p.
#'
#' @param p vector of probabilities; typically normalized, such that sum(p)=1.
#' @param b base of the logarithm (default is e)
#' @param normalize logical flag. If TRUE (default), the vector p is automatically normalized.
#'
#' @return Returns the information entropy in units that depend on b. If b=2, the units are bits; if b=exp(1), the units are nats; if b=10, the units are dits.
#'
#' @author Danail Obreschkow
#'
#' @export

entropy = function(p, b=exp(1), normalize=TRUE) {

  if (!is.vector(p)) stop('p must be a vector.')
  if (normalize) p = p/sum(p)
  return(-sum(p*log(p+1e-300))/log(b))

}
