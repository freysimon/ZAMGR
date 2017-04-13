\name{readINCABIL}
\alias{readINCABIL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read a binary INCA file
}
\description{
Read an INCA file in the BIL format from a .tar.gz archive.
}
\usage{
readINCABIL(filename, times = c("first","last","all","date"),
            date = NULL, remove = TRUE, CoSys = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{filename}{
 A character string
}
\item{times}{
 A character string
}
\item{date}{
 A POSIXct date or NULL
}
\item{remove}{
 Logical. Remove the processed files?
}
\item{CoSys}{
 A character string giving the coordiante system of the raster or NULL
}
}
\details{
Both uncompressed and compressed (.tar.gz) files can be processed. If uncompressed, only the filename without extention must be provided.
If times is "date", a POSIXct date object must be provided to selsect the date within the tim file.
If times is "all", all timestamps within the file are read and a stack of rasters is retured.
If remove = TRUE (the standard), the (uncompressed) files are deleted after they are processed. Does not affect compressed files!
If CoSys is provided (e.g. from a \link{proj4string} command), the retured raster is projected using this reference system. Otherwise no coordinate system is used.
}
\value{
A named (date and time) list with rasters
}

\author{
Simon Frey
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{raster}
\link{readBin}
\link{proj4string}
\link{crs}
}
\examples{
# file to load
file <- paste(path.package("ZAMGR"),"/extdata/INCA_TT.tar.gz",sep="")

# reading a compressed file
x <- readINCABIL(file)

# plot the results
plot(x[[1]])

# compress first and then read
file <- untar(file, exdir = tempdir())
file <- dir(path = tempdir(), pattern = ".bil", full.names = TRUE)
x <- readINCABIL(file = gsub(".bil", replacement = "", file), times = "all")

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
