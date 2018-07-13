\name{as.POSIXct.fposx}
\alias{as.POSIXct.fposx}
\title{
  Conversion from \code{fposx} to \code{POSIXct} class.
}

\description{
  Objects of class \code{fposx} are already \code{POSIXct} objects, but will always have
  the timezone set to UTC, and have an extra class attribute. Therefore the conversion
  to a purely \code{POSIXct} object is simple. The option for a timezone other than "UTC"
  is given, and the "fposx" class attribute is removed.
  Note that timezones are added as attributes only, the underlying UTC timestamp is unchanged
  (eg: UTC 10am becomes UTC+1 11am).
  If you wish to shift the underlying time (eg: UTC 10am becomes UTC+1 10am), you should either
  do so manually, or use another method.
}

\usage{
  \method{as.POSIXct}{fposx}(x, tz, ...)
}

\arguments{
  \item{x}{object of class \code{fposx}}
  \item{tz}{timezone by which to adjust output. If missing, output will have \code{tz="UTC"}.}
  \item{...}{further arguments to pass to \code{as.POSIXct}}
}
%\details{
%}
\value{
  An object of class \code{POSIXct}
}
%\references{
%}
\author{
  Ross Holmberg
}
\seealso{
  \code{as.POSIXct}
}
\examples{
  x <- fposx("2017-01-01 13:10:04")
  as.POSIXct(x)
  # [1] "2017-01-01 13:10:04 UTC"
}
