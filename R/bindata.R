#' Bin two-dimensional data in one dimension
#'
#' @importFrom stats quantile median pnorm
#'
#' @description Divides a vector of values x into finite intervals; returns the counts and other statistics in each interval.
#'
#' @param x N-element vector of x-coordinates
#' @param y optional N-element vector of values associated with the different points in x
#' @param bins If method is 'regular' or 'equal', this is a scalar specifying the number of bins. If method is 'custom' this is a vector of (n+1) x-values delimiting the n bins.
#' @param method Character string. Choose 'regular' for regularly space bins, 'equal' for bins containing an equal number of points (+-1), or 'custom' for bins with custom edges.
#' @param xlim optional 2-element vector specifying the data range (data cropped if necessary). If not given, xlim is set to the full range of x.
#'
#' @return Returns a list of items
#' \item{n}{number of bins}
#' \item{xlim}{considered range of x-coordinates, same as input argument xlim, if given}
#' \item{xleft}{n-element vector containing the x-coordinates of the left bin edges}
#' \item{xmid}{n-element vector containing the x-coordinates of the bin centres}
#' \item{xright}{n-element vector containing the x-coordinates of the right bin edges}
#' \item{dx}{n-element vector containing the widths of the bins}
#' \item{count}{n-element vector containing the number of points in each bin}
#' \item{x}{n-element vector containg the mean x-values in each bin}
#' \item{y}{n-element vector containg the mean y-values in each bin}
#' \item{xmedian}{n-element vector containg the median of the x-values in each bin}
#' \item{ymedian}{n-element vector containg the median of the y-values in each bin}
#' \item{yerr}{n-element vector giving the uncertainty on the mean}
#' \item{ysd}{n-element vector giving the standard deviations of the y-values}
#' \item{y16}{n-element vector giving the 15.86-percentile of the y-values}
#' \item{y84}{n-element vector giving the 84.13-percentile of the y-values}
#'
#' @author Danail Obreschkow
#'
#' @examples
#' # make and plot 100 random (x,y)-points
#' set.seed(1)
#' x = runif(200)
#' y = x+rnorm(200)
#' plot(x,y,pch=16,cex=0.5)
#'
#' # bin the data into 10 bins of 20 points each
#' bin = bindata(x,y,10,'equal')
#' segments(bin$xleft,bin$y,bin$xright,bin$y,col='red')
#' segments(bin$x,bin$y16,bin$x,bin$y84,col='red')
#' segments(bin$x,bin$y-bin$yerr,bin$x,bin$y+bin$yerr,col='red',lwd=3)
#' points(bin$x,bin$y,pch=16,col='red')
#'
#' @seealso \code{\link{griddata}}
#'
#' @export

bindata = function(x, y=NULL, bins=20, method='regular', xlim=NULL) {

  # handle inputs
  x = as.vector(x)
  if (!is.null(y)) {
    y = as.vector(y)
    if (length(x)!=length(y)) stop('x and y must be vectors of the same length.')
  }

  # sort data
  if (is.null(y)) {
    x = sort(x)
  } else {
    s = sort.int(x,index.return = T)
    x = s$x
    y = y[s$ix]
    s = NULL
  }

  # make limits
  if (method=='custom' & !is.null(xlim)) stop('xlim must not be specified for method "custom".')
  if (is.null(xlim)) xlim=range(x)

  # preselect x-coordinates within the range
  selection = x>=xlim[1] & x<=xlim[2]
  x = x[selection]
  if (!is.null(y)) y = y[selection]

  # make grid
  bin = list(xlim = xlim)
  if (method=='regular') {
    if (length(bins)!=1) stop('For method "regular", bins has to be a positive integer.')
    bin$n = bins
    xedges = seq(xlim[1], xlim[2], length = bin$n+1) # vector of cell-edge x-coordinates
  } else if (method=='log') {
    if (length(bins)!=1) stop('For method "log", bins has to be a positive integer.')
    if (xlim[1]<=0) stop('For method "log", the considered x-values must all be positive.')
    bin$n = bins
    xedges = exp(seq(log(xlim[1]), log(xlim[2]), length = bin$n+1)) # vector of cell-edge x-coordinates
  } else if (method=='equal') {
    if (length(bins)!=1) stop('For method "equal", bins has to be a positive integer.')
    bin$n = bins
    xedges = c(xlim[1],stats::quantile(x,probs=seq(0,1,length=bin$n+1)[2:bin$n],names=FALSE),xlim[2])
  } else if (method=='custom') {
    if (length(bins)<2) stop('For method "custom", bins as to be a vector containing the bin edges.')
    bin$n = length(bins)
    xedges = bins
  } else {
    stop('unknown method')
  }
  bin$xleft = xedges[1:bin$n]
  bin$xright = xedges[2:(bin$n+1)]
  bin$xmid = (bin$xleft+bin$xright)/2
  bin$dx = bin$xright-bin$xleft

  # make bin statistics
  index = cut(x,xedges,labels=FALSE,include.lowest=TRUE)
  tb = table(index)
  indexlist = as.numeric(names(tb))
  nindex = as.numeric(tb)
  current.index = which(index==indexlist[1])[1]
  bin$x = bin$xmedian = rep(NA,bin$n)
  if (!is.null(y)) bin$y = bin$ymedian = bin$yerr = bin$ysd = bin$y16 = bin$y84 = rep(NA,bin$n)
  bin$count = rep(0,bin$n)
  for (k in seq_along(indexlist)) {
    print
    i = indexlist[k]
    sel = seq(current.index,length=nindex[k])
    current.index = current.index+nindex[k]
    bin$count[i] = length(sel)
    bin$x[i] = mean(x[sel])
    bin$xmedian[i] = stats::median(x[sel])
    if (!is.null(y)) {
      bin$y[i] = mean(y[sel])
      bin$ymedian[i] = stats::median(y[sel])
      bin$ysd[i] = sd(y[sel])
      bin$yerr[i] = bin$ysd[i]/sqrt(bin$count[i])
      bin$y16[i] = stats::quantile(y[sel],stats::pnorm(-1),names=FALSE)
      bin$y84[i] = stats::quantile(y[sel],stats::pnorm(+1),names=FALSE)
    }
  }

  # return data
  return(bin)

}
