\name{head.growth}
\alias{head.growth}
\title{Returns first rows of a growth object.}
\description{
 Returns the first rows of a growth object.
}
\usage{
\method{head}{growth}(x, n = 6, ...)
}
\arguments{
  \item{x}{A growth object.}
  \item{n}{An integer specifying the number of lines to be shown.}
	\item{...}{other parameters to be passed through to plotting functions.}
}
\value{
  The first n rows of the growth object are printed.
}
\author{R. H. Baayen}
\seealso{See Also \code{\link{growth.fnc}}.}
\examples{\dontrun{
  data(alice)
  alice.growth = growth.fnc(alice)
  head(alice.growth)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
