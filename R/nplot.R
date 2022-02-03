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
#' @param bty character specifying the border type (see \code{\link[graphics]{par}}).
#' @param ... additional arguments used by \code{\link[graphics]{plot}}
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @export

nplot = function(xlim=c(0,1),ylim=c(0,1),xlab='',ylab='',
                 xaxs='i',yaxs='i',xaxt='n',yaxt='n',bty='n',...) {

  plot(x=NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,
       xaxs=xaxs,yaxs=yaxs,xaxt=xaxt,yaxt=yaxt,bty=bty,...)
}
