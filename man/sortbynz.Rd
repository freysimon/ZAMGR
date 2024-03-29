% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sortbynz.r
\name{sortbynz}
\alias{sortbynz}
\title{Sort by NZ}
\usage{
sortbynz(
  x,
  writefile = NULL,
  returnmatrix = TRUE,
  tz = "utc",
  dls.remove = TRUE,
  ...
)
}
\arguments{
\item{x}{xts object or character string to a file that will be read using \code{\link{read.xts}}.}

\item{writefile}{NULL or character string pointing to a file, that will be written.}

\item{returnmatrix}{logical. Should the result be returned to R?}

\item{tz}{character. Time zone.}

\item{dls.remove}{logical. Should daylight savings be removed from the time series using \link{rm.dls}}

\item{...}{additional parameters passed on to \code{\link{read.xts}}}
}
\value{
An xts object, if returnmatrix == TRUE
}
\description{
Sort an xts object by its header values (e.g. NZ)
}
\details{
This function was designed to sort an xts object created by \code{\link{make.COSERO.input}}. It can read the xts object from file or it may be fed with one directly. If x an xts object, it must contain col.names with the nz information.
}
\seealso{
\link{make.COSERO.input}; \link{read.xts}

\link{read.xts}, \link{rm.dls}
}
\author{
Simon Frey
}
