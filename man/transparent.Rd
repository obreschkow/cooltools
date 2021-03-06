% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transparent.R
\name{transparent}
\alias{transparent}
\title{Add transparency to a color}
\usage{
transparent(col, alpha = 0.5)
}
\arguments{
\item{col}{is a color or vector/array of colors, specified as text (e.g. 'purple') or 7/9-character (e.g. '#A020F0')}

\item{alpha}{transparency value between 0 and 1 or a vector/array thereof}
}
\value{
Returns a 9-character color or vector/array of 9-character colors.
}
\description{
Add transparency to a color
}
\examples{

# Add different transparencies of the same color
plot(runif(50),runif(50),pch=20,cex=10,col=transparent('purple',runif(50)))

# Add the same transparency to different colors
plot(runif(50),runif(50),pch=20,cex=10,col=transparent(rainbow(50)))

}
\seealso{
\code{\link{lightness}}
}
\author{
Danail Obreschkow
}
