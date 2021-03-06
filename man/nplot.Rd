% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nplot.R
\name{nplot}
\alias{nplot}
\title{Make empty plot area}
\usage{
nplot(
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "",
  ylab = "",
  xaxs = "i",
  yaxs = "i",
  xaxt = "n",
  yaxt = "n",
  bty = "n",
  mar = NULL,
  pty = "m",
  ...
)
}
\arguments{
\item{xlim, ylim}{vectors with plotting limits.}

\item{xlab, ylab}{horizontal and vertical labels.}

\item{xaxs, yaxs}{style of the axis interval (see \code{\link[graphics]{par}}).}

\item{xaxt, yaxt}{character which specifies the x axis type (see \code{\link[graphics]{par}}).}

\item{bty}{charater specifying the border type (see \code{\link[graphics]{par}}).}

\item{mar}{4-vector specifying the margins around the plot (bottom,left,top,right)}

\item{pty}{character specifying the type of plot region to be used; "s" generates a square plotting region and "m" generates the maximal plotting region.}

\item{...}{additional arguments used by \code{\link[graphics]{plot}}}
}
\description{
Open an empty plot
}
\author{
Danail Obreschkow
}
