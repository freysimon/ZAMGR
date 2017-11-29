#' read an ascii-raster-file
#' @param x character string pointing to a asc file
#' @param use.raster logical. Should the file be returned as raster or as matrix?
#' @param Cos (optional) character string in the format provided by \code{\link{proj4string}}
#' @author Simon Frey
#' @export
#' @import TigR
#' @import stringr
#' @import raster
#' @description reads an asc raster file and returns the resulting raster
#' @return if use.raster == TRUE a raster object is returned, else a matrix
#' @details If CoS is provided, the resulting raster is projected using the given coordinate system. If not, an unprojected raster is returned.

readASC <- function(x, CoS = NULL, use.raster = TRUE){

  #### load libraries ####
  library(TigR)
  library(stringr)
  library(raster)

  if(!substrRight(x,3) %in% c("asc","ASC")){
    stop("x must point to a asc file")
  }

  #### read asc header ####
  ascihead <- unlist(readLines(x, n = 10L))

  NODATA <- which(str_detect(ascihead, "NODATA_value"))

  ascihead <- read.table(x, nrow = NODATA, header = FALSE)

  ras <- read.table(x, skip = NODATA, header = FALSE, colClasses = "numeric")

  #### read data ####
  ras <- as.matrix(ras)

  if(use.raster){
    ncols <- as.numeric(ascihead[which(ascihead[,1] == "ncols"),2])
    nrows <- as.numeric(ascihead[which(ascihead[,1] == "nrows"),2])
    xll <- as.numeric(ascihead[which(ascihead[,1] == "xllcorner"),2])
    yll <- as.numeric(ascihead[which(ascihead[,1] == "yllcorner"),2])
    cellsize <- as.numeric(ascihead[which(ascihead[,1] == "cellsize"),2])
    NODATA <- as.numeric(ascihead[which(ascihead[,1] == "NODATA_value"),2])


    #### calculate extent ####
    rasext <- extent(c(xll, xll + ncols * cellsize, yll, yll + nrows * cellsize))

    #### set extent ####
    ras <- raster(ras)
    ras <- setExtent(ras, rasext)

    if(!is.null(CoS)){
      proj4string(ras) <- CoS
    }
  }

  #### return raster ####
  return(ras)
}
