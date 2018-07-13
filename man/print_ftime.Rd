\name{print.ftime}
\alias{print.ftime}
\title{
  Print method for \code{ftime} class objects.
}

\description{
  A slightly modified version of \code{print.times} from the `chron`
  package by David James and Kurt Hornik
  \cite{https://CRAN.R-project.org/package=chron}. The main difference
  here is that millisecond precision can be printed.
}

\usage{
  \method{print}{ftime}(x, ...)
}

\arguments{
  \item{x}{object of class \code{ftime}}
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
