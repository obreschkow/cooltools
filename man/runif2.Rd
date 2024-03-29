% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runif2.R
\name{runif2}
\alias{runif2}
\title{Generate randomly oriented vectors in 2D}
\usage{
runif2(n = 1, r = c(0, 1), azimuth = c(0, 2 * pi), quasi = FALSE, start = 1)
}
\arguments{
\item{n}{number of random vectors to be generated}

\item{r}{2-vector specifying the range of radii}

\item{azimuth}{2-vector specifying the range of azimuth angles}

\item{quasi}{logical flag. If true, quasi-random numbers with low-discrepancy are drawn, based on a Halton sequence. Otherwise, the standard internal pseudo-random generator of \code{runif()} is used.}

\item{start}{starting index of Halton sequence. Only used if \code{quasi=TRUE}.}
}
\value{
Returns an n-by-2 array of n vectors.
}
\description{
Generate randomly oriented vectors in 2D, following an isotropic distribution (optionally truncated to a region).
}
\examples{
## generate 500 unit vectors with radii between 0.5 and 1
x = runif2(500,r=c(0.5,1))
oldpar = par(pty='s')
plot(x,pch=20)
par(oldpar)

}
\seealso{
\code{\link{runif3}}
}
\author{
Danail Obreschkow
}
