\name{ftime}
\alias{ftime}
\title{
  Fast version of \code{chron::times}.
}
\description{
  It is extremely fast (compared to \code{as.Date} by several orders
  of magnitude since it uses pure text parsing and no system calls.
}
\usage{
ftime(x, required.components = 3L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{string vector to interpret as dates}
  \item{required.components}{minimum number of timestamp components that
    are required. For example 2 means only the year and month are required.
    If the requirement is not met, the result for that entry will be \code{NA}.}
}
%\details{
%}
\value{
  Numeric vector of the class \code{times}.
}
%\references{
%}
\author{
  Simon Urbanek
}
\seealso{
  \code{chron::times}
}
\examples{
  ## let us generate a bunch of random timestamps until today
  ts <- as.character(data.table::as.ITime(chron::times(seq.int(0,1,length.out = 1E5))))

  ## convert them using as.POSIXct
  system.time(a <- chron::times(ts))

  ## same using the fast method
  system.time(b <- ftime(ts))

  # very slight differences in results exist, but they are negligible
  # the sum total difference for this 100,000 samples is less than 1 microsecond
  sum( abs( as.numeric( a ) - as.numeric( b ) ) ) < ( 1E-6 / 86400 )
}
