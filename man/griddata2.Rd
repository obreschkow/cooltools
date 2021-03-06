% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/griddata2.R
\name{griddata2}
\alias{griddata2}
\title{Grid 2D point set}
\usage{
griddata2(x, y = NULL, w = NULL, n = c(20, 20), xlim = NULL, ylim = NULL)
}
\arguments{
\item{x}{N-element vector of x-coordinates or N-by-2 matrix of (x,y)-coordinates}

\item{y}{N-element vector of y-coordinates (only used if x is a vector)}

\item{w}{optional N-element vector with weights}

\item{n}{scalar or 2-element vector specifying the number of equally space grid cells along each direction.}

\item{xlim}{2-element vector specifying the x-range (data cropped if necessary). If not given, xlim is set to the range of x.}

\item{ylim}{2-element vector specifying the y-range (data cropped if necessary). If not given, ylim is set to the range of y.}
}
\value{
Returns a list of items
\item{x}{n-element vector of cell-center x-coordinates}
\item{y}{n-element vector of cell-center y-coordinates}
\item{xbreak}{(n+1)-element vector of cell-edge x-coordinates}
\item{ybreak}{(n+1)-element vector of cell-edge y-coordinates}
\item{dx}{spacing between x-coordinates}
\item{dy}{spacing between y-coordinates}
\item{xlim}{range of xbreak, same as input argument xlim, if given}
\item{ylim}{range of ybreak, same as input argument ylim, if given}
\item{n}{2D array of point counts.}
\item{d}{normalized number densities corresponding to n, such that \code{sum(d)*dx*dy=1}.}
\item{m}{2D array of weighted point counts (masses); only available if \code{w} is specified.}
\item{c}{2D array of normalized mass densities corresponding to m, such that sum(c)*dx*dy=1; only available if \code{w} is specified.}
}
\description{
Generates a 2D grid from a 2D point set, optionally with weights
}
\examples{
set.seed(1)
x = runif2(200)
counts = griddata2(x,xlim=c(-1,1),ylim=c(-1,1))$n
nplot(pty='s',xlim=c(-1,1),ylim=c(-1,1))
rasterImage(rasterflip(lim(counts,0,3)/3),-1,-1,1,1,interpolate=FALSE)
points(x,col='red',pch=20,cex=0.5)

}
\seealso{
\code{\link{griddata}}
}
\author{
Danail Obreschkow
}
