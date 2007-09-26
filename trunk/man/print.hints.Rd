\name{print.hints}
\alias{print.hints}
\title{ Print method for hints objects }
\description{
This function is the print method for hints objects
}
\usage{
\method{print}{hints}(x, outFile = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Any object that has an S3 class }
  \item{outFile}{The name of the output file; the default is to output to
  the console}
  \item{\dots}{additional arguements are passed to \code{formatDL} which is used to 
  format the output }
}

\value{Prints a list of functions that can be used with a specified object.}

\author{Hadley Wickham,
\email{h.wickham@gmail.com} and Sanford Weisberg,
\email{sandy@stat.umn.edu}}

\seealso{\code{\link{formatDL}}, \code{\link{hints}}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }

