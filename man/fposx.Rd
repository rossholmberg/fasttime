\name{fposx}
\alias{fposx}
\title{
  Conversion of timestamp values to \code{fposx}.
}
\description{
  \code{fposx} converts character timestamps into objects with both
  \code{fposx} and \code{POSIXct} class attributes.
  
  Timezone (\code{tzone}) attribute is retained for compatibility with
  \code{POSIXct}, but is always set to "UTC", and is not displayed in the
  print method. The intention is for users to handle timezone issues
  themselves, with R staying "out of the way". The \code{fposx} class is intended
  to be used without a meaningful timezone attribute. The user is responsible
  for time-shift modifications according to timezone and daylight saving changes.
  
  \code{fposx} interprets sequences of digits separated by non-digits as a
  timestamp in UTC. The order of interpretation is fixed: year, month, day,
  hour, minute, second. Note that only true (positive) POSIX dates
  (since 1970-01-01 00:00:00) are supported and \code{fposx}
  accepts dates up to year 2199.

  It is extremely fast (compared to \code{as.POSIXct} by several orders
  of magnitude (on some platfroms 1000x faster) since it uses pure text
  parsing and no system calls.
  
  The code here is a modified version of Simon Urbanek's excellent
  \code{fastPOSIXct} function \cite{http://www.rforge.net/fasttime}.
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
  Ross Holmberg
}
\seealso{
  \code{\link{as.POSIXct}}
}
\examples{
  ## let us generate a bunch of random timestamps until today
  ts <- as.character(.POSIXct(runif(1e5) * unclass(Sys.time()), "GMT"))

  ## convert them using as.POSIXct
  system.time(a <- as.POSIXct(ts, "UTC"))
  #    user  system elapsed 
  # 0.738   0.907   1.648 

  ## same using the fast method
  system.time(b <- fposx(ts))
  #    user  system elapsed 
  # 0.006   0.000   0.006 

  # Note the timezone is not printed.
  b[1]
  # [1] "2007-08-04 14:30:37"
  
  # It is present, but is always "UTC"
  attr( b, "tzone" )
  # [1] "UTC"
  
  # An `fposx` object is also a `POSIXct` object
  inherits( b, "POSIXct" )
  # [1] TRUE
  
  # And can therefore be printed using the base function if needed.
  print.POSIXct(b[1])
  # [1] "2000-10-01 09:30:04 UTC"

  # `fposx` object has an extra class attribute, but is otherwise identical
  attr(b, "class") <- attr(a, "class")
  identical( a, b )
  # [1] TRUE
}
