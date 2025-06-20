\name{xylowess.fnc}
\alias{xylowess.fnc}
\title{Trellis scatterplot with smoothers}
\description{
  Convenience function for trellis scatterplots with smoothers added.
}
\usage{
xylowess.fnc(fmla, data, 
  span = 2/3, symbolcolor = "darkgrey", 
  linecolor = "blue", xlabel = "", ylabel = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmla}{A formula.}
  \item{data}{A dataframe.}
  \item{span}{Span for the smoother.}
  \item{symbolcolor}{Color for plot symbols.}
  \item{linecolor}{Color for smoother.}
  \item{xlabel}{Label for horizontal axis.}
  \item{ylabel}{Label for vertical axis.}
  \item{...}{Arguments to be passed to methods.}
}
\value{
  A trellis scatterplot matrix with smoothers is shown on the graphics device.
  Code builds on xyplot from the lattice package.
}
\author{R. H. Baayen}
%\seealso{See also \code{\link[lattice]{xyplot}}.}
\examples{\dontrun{
  data(weightRatings)
  xylowess.fnc(Rating ~ Frequency | Subject, data = weightRatings,
    xlab = "log Frequency", ylab = "Weight Rating")
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
