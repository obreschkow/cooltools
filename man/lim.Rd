% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lim.R
\name{lim}
\alias{lim}
\title{Crop values of vector or array to a custom range}
\usage{
lim(x, min = 0, max = 1, clip = NULL, na = NULL)
}
\arguments{
\item{x}{vector or array}

\item{min}{minimum value}

\item{max}{maximum value}

\item{clip}{optional value specifying the value assigned to clipped data, e.g. \code{clip=NA}}

\item{na}{optional value specifying the value assigned to non-numbers (NA and NaN)}
}
\value{
vector/array of the same shape as \code{x}
}
\description{
limits the values of a vector or array to a desired interval, while keeping the shape of the vector/array
}
\author{
Danail Obreschkow
}
