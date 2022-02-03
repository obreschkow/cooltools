#' Insert a sub-panel into plot
#'
#' @importFrom graphics par plot curve rect
#'
#' @description Insert a sub-panel into an existing plotting area. To open a subplot, call \code{par(subplot(...))}, to close it, you must call \code{par(subplot('off'))}.
#'
#' @param xleft lower x-coordinate of the sub-panel relative to the current plot.
#' @param ybottom lower y-coordinate of the sub-panel relative to the current plot.
#' @param xright upper x-coordinate of the sub-panel relative to the current plot.
#' @param ytop upper y-coordinate of the sub-panel relative to the current plot.
#'
#' @return graphical parameters to user with \code{\link[graphics]{par}}.
#'
#' @author Danail Obreschkow
#'
#' @examples
#' # main plot
#' f = function(x) x*sin(1/(x+.Machine$double.eps))
#' curve(f,0,1,n=1000,ylim=c(-1,1),xlab='',ylab='',xaxs='i',yaxs='i')
#' rect(0.02,-0.1,0.1,0.1,border='blue',col=transparent('blue',0.2))
#'
#' # subplot
#' par(subplot(0.55,-0.8,0.93,0))
#' curve(f,0.02,0.1,n=500,ylim=c(-0.1,0.1),xlab='',ylab='',xaxs='i',yaxs='i')
#' rect(0.02,-0.1,0.1,0.1,border=NA,col= transparent('blue',0.2))
#' par(subplot('off'))
#'
#' @export

subplot = function(xleft=NA, ybottom, xright, ytop) {

  # safe use of par()
  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if (is.numeric(xleft)) {

    # check if coordinates of subplot fall within the user limits of current plot
    limit = par()$usr
    if ((xleft<limit[1])|(xleft>limit[2])) stop('xleft out of its allowed range')
    if ((xright<limit[1])|(xright>limit[2])) stop('xright out of its allowed range')
    if ((ybottom<limit[3])|(ybottom>limit[4])) stop('ybottom out of its allowed range')
    if ((ytop<limit[3])|(ytop>limit[4])) stop('ytop out of its allowed range')
    if (xleft>=xright) stop('xleft must be smaller than xright')
    if (ybottom>=ytop) stop('ybottom must be smaller than ytop')

    # store previous subplot
    .cooltools.env$master.par = par(no.readonly = TRUE)

    # recast ranges to [0,1]
    xleft = (xleft-limit[1])/(limit[2]-limit[1])
    xright = (xright-limit[1])/(limit[2]-limit[1])
    ybottom = (ybottom-limit[3])/(limit[4]-limit[3])
    ytop = (ytop-limit[3])/(limit[4]-limit[3])

    # define new subplot
    par(omd=c(0,1,0,1))
    xmarg = sum(par()$mai[c(2,4)])
    xplot = par()$pin[1]
    ymarg = sum(par()$mai[c(1,3)])
    yplot = par()$pin[2]
    omd = c(xleft*xplot/(xplot+xmarg),(xright*xplot+xmarg)/(xplot+xmarg),ybottom*yplot/(yplot+ymarg),(ytop*yplot+ymarg)/(yplot+ymarg))
    return(list(new=TRUE, omd=omd))

  } else {

    if (xleft=='off') {

      # close subplot
      if (is.null(.cooltools.env$master.par)) stop('subplot("off") called without an open subplot')
      return(.cooltools.env$master.par)

    } else {

      stop('xleft must be either a numeric value or "off"')

    }

  }
}
