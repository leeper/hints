\name{xtable.hints}
\alias{xtable.hints}
\title{ xtable  method for hints objects }
\description{
Format output from hints for latex printing.
}
\usage{
\method{xtable}{hints}(x, align="llll",...)
}
\arguments{
  \item{x}{ An object of class hints created by call to hints }
  \item{align}{xtable output is in four columns.  The default is to use
left alignment.}
  \item{\dots}{additional arguments passed to xtable }
}

\value{The output from hints is reformatted and the output using
\code{xtable.data.frame} in the xtable package.}

\author{Hadley Wickham,
\email{h.wickham@gmail.com} and Sanford Weisberg,
\email{sandy@stat.umn.edu}}

\seealso{\code{\link{hints}}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }

