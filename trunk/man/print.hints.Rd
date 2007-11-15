\name{print.hints}
\alias{print.hints}
\title{ Print method for hints objects }
\description{
This function is the print method for hints objects
}
\usage{
\method{print}{hints}(x, outFile="window",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Any object that has an S3 class }
  \item{outFile}{The default value of "window" displays the output in a separate
  window.  Any other string value displays the output in the standard output window.}
  \item{\dots}{additional arguments not used by this print method }
}

\value{Prints a list of functions that can be used with a specified object.}

\author{Hadley Wickham,
\email{h.wickham@gmail.com} and Sanford Weisberg,
\email{sandy@stat.umn.edu}}

\seealso{\code{\link{hints}}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }

