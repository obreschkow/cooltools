#' Insert a sub-panel into plot
#'
#' @importFrom graphics par plot curve rect
#'
#' @description Insert a sub-panel into an existing plotting area. Terminate the sub-panel using by calling \code{subplot('off')}
#'
#' @param xleft lower x-coordinate of the sub-panel relative to the range 0...1. Use xleft='off' to return to the main plot.
#' @param ybottom lower y-coordinate of the sub-panel relative to the range 0...1.
#' @param xright upper x-coordinate of the sub-panel relative to the range 0...1.
#' @param ytop upper y-coordinate of the sub-panel relative to the range 0...1.
#'
#' @author Danail Obreschkow
#'
#' @examples
#' nplot(bty='o', xlim=c(0,1), ylim=c(-1,1), xaxt ='t',yaxt='t')
#' curve(x*sin(1/x), 1e-10, 1, n=1000, lwd=1.5, add=TRUE)
#' rect(0.02,-0.1,0.1,0.1,border='blue',col=transparent('blue',0.2))
#' subplot(0.55,0.15,0.93,0.55)
#' nplot(bty='o', xlim=c(0.02,0.1), ylim=c(-0.1,0.1), xaxt ='t',yaxt='t')
#' rect(0.02,-0.1,0.1,0.1,border = NA,col= transparent('blue',0.2))
#' curve(x*sin(1/x), 0.02, 0.1, n=500, lwd=1.5, add=TRUE)
#' subplot('off')
#'
#' @export

subplot = function(xleft=0.1, ybottom=0.1, xright=0.3, ytop=0.3) {

  if (is.numeric(xleft)) {

    if ((xleft<0)|(xleft>1)) stop('xleft must be in the range 0...1')
    if ((xright<0)|(xright>1)) stop('xright must be in the range 0...1')
    if ((ybottom<0)|(ybottom>1)) stop('ybottom must be in the range 0...1')
    if ((ytop<0)|(ytop>1)) stop('ytop must be in the range 0...1')
    if (xleft>=xright) stop('xleft must be smaller than xright')
    if (ybottom>=ytop) stop('ybottom must be smaller than ytop')

    # define new subplot
    par(omd=c(0,1,0,1))
    xmarg = sum(par()$mai[c(2,4)])
    xplot = par()$pin[1]
    ymarg = sum(par()$mai[c(1,3)])
    yplot = par()$pin[2]
    par(new=T,omd=c(xleft*xplot/(xplot+xmarg),(xright*xplot+xmarg)/(xplot+xmarg),ybottom*yplot/(yplot+ymarg),(ytop*yplot+ymarg)/(yplot+ymarg)))

  } else {

    if (xleft=='off') {

      # close subplot
      par(oma=c(0,0,0,0))
      par(omi=c(0,0,0,0))
      par(omd=c(0,1,0,1))
      par(new=F)

    } else {

      stop('xleft must be either a numeric value or "off"')

    }

  }
}
