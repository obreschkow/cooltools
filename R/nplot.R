#' Make empty plot area
#'
#' @importFrom graphics par plot
#'
#' @description Open an empty plot
#'
#' @param xlim,ylim vectors with plotting limits.
#' @param xlab,ylab horizontal and vertical labels.
#' @param xaxs,yaxs style of the axis interval (see \code{\link[graphics]{par}}).
#' @param xaxt,yaxt character which specifies the x axis type (see \code{\link[graphics]{par}}).
#' @param bty charater specifying the border type (see \code{\link[graphics]{par}}).
#' @param mar 4-vector specifying the margins around the plot (bottom,left,top,right)
#' @param pty character specifying the type of plot region to be used; "s" generates a square plotting region and "m" generates the maximal plotting region.
#' @param ... additional arguments used by \code{\link[graphics]{plot}}
#'
#' @author Danail Obreschkow
#'
#' @export

nplot = function(xlim=c(0,1),ylim=c(0,1),xlab='',ylab='',
                 xaxs='i',yaxs='i',xaxt='n',yaxt='n',bty='n',
                 mar=NULL,pty='m',...) {

  if (!is.null(mar)) par(mar=mar)
  par(pty=pty)
  plot(x=NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,
       xaxs=xaxs,yaxs=yaxs,xaxt=xaxt,yaxt=yaxt,bty=bty,...)
}
