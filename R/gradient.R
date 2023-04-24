#' Compute gradient
#'
#' @description Evaluates the gradient of a vector of array of rank 2 or 3
#'
#' @param f vector or array of rank 2 or 3
#' @param circular logical scalar or vector specifying whether periodic boundaries are used along each dimension
#'
#' @return returns a list with the different components of the gradient; each component has the same dimension as f.
#'
#' @author Danail Obreschkow
#'
#' @export

gradient = function(f, circular=FALSE) {

  f = as.array(f)
  n = dim(f)
  d = length(n)
  if (length(circular)==1) circular=rep(circular,d)

  if (d==1) {
    g = 0.5*(cshift(f,-1)-cshift(f,1))
    if (!circular) {
      g[1] = f[2]-f[1]
      g[n] = f[n]-f[n-1]
    }
  } else if (d==2) {
    g = list(x = 0.5*(cshift(f,c(-1,0))-cshift(f,c(1,0))),
             y = 0.5*(cshift(f,c(0,-1))-cshift(f,c(0,1))))
    if (!circular[1]) {
      g$x[1,] = f[2,]-f[1,]
      g$x[n[1],] = f[n[1],]-f[n[1]-1,]
    }
    if (!circular[2]) {
      g$y[,1] = f[,2]-f[,1]
      g$y[,n[2]] = f[,n[2]]-f[,n[2]-1]
    }
  } else if (d==3) {
    g = list(x = 0.5*(cshift(f,c(-1,0,0))-cshift(f,c(1,0,0))),
             y = 0.5*(cshift(f,c(0,-1,0))-cshift(f,c(0,1,0))),
             z = 0.5*(cshift(f,c(0,0,-1))-cshift(f,c(0,0,1))))
    if (!circular[1]) {
      g$x[1,,] = f[2,,]-f[1,,]
      g$x[n[1],,] = f[n[1],,]-f[n[1]-1,,]
    }
    if (!circular[2]) {
      g$y[,1,] = f[,2,]-f[,1,]
      g$y[,n[2],] = f[,n[2],]-f[,n[2]-1,]
    }
    if (!circular[3]) {
      g$z[,,1] = f[,,2]-f[,,1]
      g$z[,,n[3]] = f[,,n[3]]-f[,,n[3]-1]
    }
  } else {
    stop('gradient function only works for arrays of rank 1,2,3')
  }

  return(g)

}
