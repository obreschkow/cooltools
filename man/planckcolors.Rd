% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/planckcolors.R
\name{planckcolors}
\alias{planckcolors}
\title{Planck CMB colour palette}
\usage{
planckcolors(n)
}
\arguments{
\item{n}{integer number of colors in the scale}
}
\value{
Returns an n-vector of RGB colour strings.
}
\description{
Generates color scale matching the one normally used to display the Planck CMB temperature map from -300uK to +300uK.
}
\author{
Danail Obreschkow

#' @examples
nplot()
rasterImage(rbind(planck.colors(1e3)),0,0,1,1)
}
