\name{ftime}
\alias{ftime}
\title{
  Fast and precise conversion of character time values.
}
\description{
  Similar to \code{times} from the \code{chron} package, but faster, and
  prints millisecond values when appropriate.
  The code here is a modified version of Simon Urbanek's excellent
  \code{fastPOSIXct} function \cite{http://www.rforge.net/fasttime}.
}
\usage{
ftime(x, required.components = 3L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{string vector to interpret as times}
  \item{required.components}{minimum number of timestamp components that
    are required. For example 2 means only the hours and seconds are required.
    If the requirement is not met, the result for that entry will be \code{NA}.}
}
%\details{
%}
\value{
  Numeric vector of the class \code{ftime}, which also includes class \code{times}.
}
%\references{
%}
\author{
  Ross Holmberg
}
\examples{
  ## let us generate a bunch of random times, with millisend precision
  set.seed(42)
  n <- 1E5
  hrs <- formatC( sample( 0:23, n, replace = TRUE ), width = 2, flag = "0" )
  min <- formatC( sample( 0:59, n, replace = TRUE ), width = 2, flag = "0" )
  sec <- formatC( sample( 0:59, n, replace = TRUE ), width = 2, flag = "0" )
  ms <- formatC( sample( 0:999, n, replace = TRUE ), width = 3, flag = "0" )
  ts <- paste0( hrs, ":", min, ":", sec, ".", ms )

  ## convert them using chron::times
  # system.time(a <- chron::times(ts))

  ## same using the fast method
  system.time(b <- ftime(ts))
  
  # ftime objects print with ms, unlike `times` objects
  head(b,3)
  # [1] 21:42:21     22:20:11.015 06:05:29.152

  # very slight differences in results exist, but they are negligible
  # the sum total difference for this 100,000 samples is less than 1 microsecond
  # sum( abs( as.numeric( a ) - as.numeric( b ) ) ) < ( 1E-6 / 86400 )
  # TRUE
}
