\name{hints}
\alias{hints}
\title{List functions that act on an object  }
\description{
Once you create an object of a particular type, remembering the functions
that exist to act on that object can be difficult.  This function provides a
simple list of these functions.
}
\usage{
%hints(x, class = base::class(x), load.libraries = FALSE)
hints(x, class = base::class(x))
}
\arguments{
  \item{x}{Any R object.  Either \code{x} or \code{class} must be supplied. }
  \item{class}{A class of objects; the default is the class of
\code{x}. }
%  \item{load.libraries}{If TRUE, look in all packages in the library for
%functions to list; if false, use only packages that have already been
%loaded. }
}
}
\value{
This function returns an object of class \code{c("hints","data.frame")}
giving a list of functions that accept objects of the specified class, the
package that includes the function, and 
a short description of what that function does.  The descriptions are taken
from the title entry of the documentation for the function.

The \code{print} function prints the output in a sensible way.  All the arguments
to \code{print.data.frame} can be used.  The object
is compatible with the \code{xtable} package to get LaTeX output, and all the
arguments to \code{xtable} can be used as well.

}
\author{This function was written by Hadley Wickham,
\email{h.wickham@gmail.com}, based on a suggestion by Sanford Weisberg at
the useR! 2006 conference in Vienna. 
Minor changes were made by Sanford Weisberg,
\email{sandy@stat.umn.edu}}

%\section{Warning }{The argument \code{load.packages=TRUE} loads all the
%packages in your library, and if loading a library has any side effects
%these side effects will occur, and you may get an error.
%}
\examples{
x <- rnorm(10)
y <- rnorm(10)
m <- lm(y ~ x)
hints(m)
hints(class="glm")
\dontrun{
library(xtable)
xtable(hints(m))
}
}
\keyword{ methods }
