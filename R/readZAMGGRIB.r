#' Read a GRIB-1 file using wgrib
#' @description Read a GRIB-1 file provided by the ZAMG using wgrib and extract a specific raster set.
#' @author Simon Frey
#' @import raster
#' @import TigR
#' @export
#' @param gribfile Absolute path to a gribfile
#' @param ex Either an \code{\link{extent}} object, NULL, alaro, or arome. As of now, arome is not supported, yet.
#' @param crs Either a \code{\link{crs}} string, NULL, alaro, or arome. As of now, arome is not supported, yet.
#' @param wgrib Absolute path to the executable of wgrib. If NULL, wgrib must be installed properly on the system.
#' @param level numeric giving the record number of variable within GRIB file
#' @return A (projected) raster with the extracted information
#' @details If ex and/or crs are supplied, the returned raster will be projected. If ex and/or crs are specified as
#' "alaro" or "arome", the respective extent and crs are used. Note, that as of now, only alaro is supported.
#'
#'     This function relies on the package \code{\link{TigR}}


readZAMGGRIB <- function(gribfile, ex = NULL, crs = NULL, wgrib = NULL, level){

  if(is.null(wgrib)){
    wgrib <- "wgrib"
  } else if(!is.character(wgrib)){
    stop("wgrib must be the path to wgrib.exe You may download it from: http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html")
  }

  if(!is.numeric(level)){
    stop("level must be an integer giving the record number of variable within GRIB file")
  }

  if(dirname(gribfile) == "."){
    stop("gribfile must be given with an absolute path")
  }

  if(!is.null(crs)){
    if(crs %in% c(tolower("alaro"),toupper("alaro"),"Alaro")){
      crs <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    }
  }

  if(!is.null(ex)){
    if(ex %in% c(tolower("alaro"),toupper("alaro"),"Alaro"))
      ex <- extent(2.0, 2.0+508*0.05999999865889549, 55.11000097543001-446*0.03999999910593033,55.11000097543001)
  }



  library(TigR)
  library(raster)

  # executing wgrib to extract the date to a tempdir
  tmp <- tempfile(fileext = ".txt")

  wstring <- paste(changeSlash(wgrib), changeSlash(gribfile) ,"-d", level, "-text -o", tmp, sep = " ")
  system(wstring)

  # reading tempfile
  dims <- read.table(tmp, header = FALSE, nrow = 1)
  IN <- read.table(tmp, header = FALSE, skip = 1)
  file.remove(tmp)

  # creating matrix
  mat1 <- matrix(ncol = dims[,1], nrow = dims[,2], data = NA)

  rows <- seq(1,(dims[,1]*dims[,2]), dims[,1])
  rows0 <- rows-1
  rows0 <- c(rows0[-1], dims[,1]*dims[,2])

  # fill matrix
  for(k in 1:length(rows)){
    mat1[k,] <- IN[rows[k]:rows0[k],]
  }

  # creating raster from matrix
  ras <- raster(mat1)

  if(!is.null(ex)){
    ras <- setExtent(ras, ex)
  }

  if(!is.null(crs)){
    crs(ras) <- crs
  }



  return(ras)

}
