\name{fposx}
\alias{fposx}
\title{
  Fast version of \code{as.POSIXct.character} for GMT fixed format.
}
\description{
  \code{fposx} converts timestamps in textual (string) form into
  objects with both \code{fposx} and \code{POSIXct} class attributes.
  Timezone (\code{tzone}) attribute is retained for compatibility with
  \code{POSIXct}, but is always set to "UTC", and is not displayed in the
  print method. The intention is for users to handle timezone issues
  themselves, with R staying "out of the way".
  \code{fposx} interprets sequences of digits separated by non-digits as a
  timestamp in GMT. The order of interpretation is fixed: year, month, day,
  hour, minute, second. Note that only true (positive) POSIX dates
  (since 1970-01-01 00:00:00) are supported and \code{fposx}
  accepts dates up to year 2199.

  It is extremely fast (compared to \code{as.POSIXct} by several orders
  of magnitude (on some platfroms 1000x faster) since it uses pure text
  parsing and no system calls.
}
\usage{
fposx(x, required.components = 3L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{string vector to interpret as timestamps}
  \item{required.components}{minimum number of timestamp components that
    are required. For example 3 means only the date is required, 6 means
    all components (up to the seconds) are required. If the requirement
    is not met, the result for that entry will be \code{NA}.}
}
%\details{
%}
\value{
  Numeric vector of the class \code{fposx} and \code{POSIXct}. In fact
  this function computes the numeric vector and calls \code{.POSIXct} to
  create the result, then adds \code{fposx} as a class attribute.
  The timezone is present to maintain compatibility with \code{POSIXct}, but
  is always set to "UTC".
}
%\references{
%}
\author{
  Simon Urbanek
}
\seealso{
  \code{\link{as.POSIXct}}
}
\examples{
  ## let us generate a bunch of random timestamps until today
  ts <- as.character(.POSIXct(runif(1e4) * unclass(Sys.time()), "GMT"))

  ## convert them using as.POSIXct
  system.time(a <- as.POSIXct(ts, "GMT"))

  ## same using the fast method
  system.time(b <- fposx(ts, "GMT"))

  all.equal(a, b)
}
\keyword{chron}
