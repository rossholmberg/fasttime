\name{print.fposx}
\alias{print.fposx}
\title{
  Print method for \code{fposx} class objects.
}

\description{
   Same as \code{print.POSIXct}, but without the timezone.
   
   The \code{fposx} class is intended to be used without a meaningful timezone
   attribute. The user is responsible for time-shift modifications according to
   timezone and daylight saving changes.
}

\usage{
  \method{print}{fposx}(x, ...)
}

\arguments{
  \item{x}{fposx vector to print}
  \item{...}{further arguments to pass on}
}
%\details{
%}
\value{
  Input object is invisibly returned unchanged.
}
%\references{
%}
\author{
  Ross Holmberg
}
\seealso{
  \code{fposx}
}
\examples{
  x <- fposx( "2000-10-01 09:30:04" )
  print.fposx( x )
  # [1] "2000-10-01 09:30:04"
}
