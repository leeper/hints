\name{hints}
\alias{hints}
\title{List functions that act on an object  }
\description{
Once you create an object of a particular type, remembering the functions
that exist to act on that object can be difficult.  This function provides a
simple list of these functions.
}
\usage{
hints(x, class = base::class(x), all.packages=FALSE)
}
\arguments{
  \item{x}{Any R object.  Either \code{x} or \code{class} must be supplied. }
  \item{class}{A class of objects; the default is the class of
\code{x}. }
  \item{all.packages}{If TRUE, look in all available packages for
functions to list; if FALSE (the default), use only packages that have already 
been loaded. }
}
\value{
This function returns an object with S3 class \code{c("hints")}
giving a list of functions that accept objects of the specified class, the
package that includes the function, and 
a short description of what that function does.  The descriptions are taken
from the title entry of the documentation for the function. The print method will put the 
results in a separate window.

The object
is compatible with the \code{xtable} package to get LaTeX output, and all the
arguments to \code{xtable} can be used.

}
\author{This function was written by Hadley Wickham,
\email{h.wickham@gmail.com}, based on a suggestion by Sanford Weisberg at
the useR! 2006 conference in Vienna. 
Minor changes were made by Sanford Weisberg,
\email{sandy@stat.umn.edu}}

\seealso{\code{\link{print.hints}}}

\examples{
\dontrun{
m <- lm(BOD)
hints(m)
library(xtable)
xtable(hints(m))
}
}
\keyword{ methods }
