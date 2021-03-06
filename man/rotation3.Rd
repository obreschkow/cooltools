% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rotation3.R
\name{rotation3}
\alias{rotation3}
\title{3D rotation matrix}
\usage{
rotation3(u, angle = NULL)
}
\arguments{
\item{u}{3-vector specifying the rotation axis}

\item{angle}{rotation angle in radians; if not given, the norm of the vector \code{u} is interpreted as the angle}
}
\value{
Returns a 3-by-3 rotation matrix
}
\description{
Compute a 3D rotation matrix given an axis and an angle
}
\seealso{
\code{\link{rotation2}}
}
\author{
Danail Obreschkow
}
