\name{tail.growth}
\alias{tail.growth}
\title{Show last rows of growth object.}
\description{
  Prints last rows of growth object.
}
\usage{
\method{tail}{growth}(x, n = 6, ...)
}
\arguments{
  \item{x}{A growth object.}
  \item{n}{An integer specifying the number of lines to be shown.}
	\item{...}{other parameters to be passed through to plotting functions.}
}
\value{
  The last n rows of the growth object are printed.
}
\author{R. H. Baayen}
\seealso{See Also \code{\link{growth.fnc}}.}
\examples{\dontrun{
  data(alice)
  alice.growth = growth.fnc(alice)
  tail(alice.growth)
}}
\keyword{internal}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
