#' Read a GRIB-1 file using wgrib
#' @description Read a GRIB-1 file provided by the ZAMG using wgrib and extract a specific raster set.
#' @author Simon Frey
#' @import raster
#' @import TigR
#' @export
#' @param gribfile Absolute path to a gribfile
#' @param ex Either an \code{\link{extent}} object, NULL, 'alaro', or 'arome'.
#' @param crs Either a \code{\link{crs}} string, NULL, 'alaro', or 'arome'.
#' @param wgrib Absolute path to the executable of wgrib. If NULL, wgrib must be installed properly on the system.
#' @param recnr numeric giving the record number of the respective variable within GRIB file
#' @param variable character string giving the abbreviated variable within the GRIB file.
#' @param remove either TRUE/FALSE for removing / not removing the gribfile after reading
#'  or ''success'/fail' for only removing files that have / haven't been successfully read.
#' @return A (projected) raster with the extracted information
#' @seealso For reading INCA files, see \code{\link{readINCABIL}}
#' @details If ex and/or crs are supplied, the returned raster will be projected. If ex and/or crs are specified as
#' "alaro" or "arome", the respective extent and crs are used.
#'
#'     Either recnr or variable must be specified to extract the information of interest.
#'     If both are given the latter is ignored.
#'
#'     This function relies on the package TigR, which is available on \href{https://github.com/freysimon/TigR}{Github}
#'
#'     Note that wgrib (64Bit) must be installed on the machine. It can be downloaded from \url{http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}
#'
#'     ZAMG per default flips their rasters by "y", so the output will be fliped back that the raster is proceted correcly.

readZAMGGRIB <- function(gribfile, ex = NULL, crs = NULL, wgrib = NULL, recnr = NULL, variable = NULL, remove = FALSE){

  if(all(is.null(recnr), is.null(variable))){
    stop("Either recnr or variable must be specified")
  }
  if(!any(is.null(recnr), is.null(variable))){
    variable <- NULL
  }
  if(is.null(wgrib)){
    wgrib <- "wgrib"
  } else if(!is.character(wgrib)){
    stop("wgrib must be the path to wgrib.exe You may download it from: http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html")
  }

  if(!is.numeric(recnr) & is.null(variable)){
    stop("recnr must be an integer giving the record number of variable within GRIB file")
  }

  if(dirname(gribfile) == "."){
    stop("gribfile must be given with an absolute path")
  }

  if(!is.null(crs)){
    if(crs %in% c(tolower("alaro"),toupper("alaro"),"Alaro",
                  tolower("arome"),toupper("arome"),"Arome")){
      crs <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    }
  }

  is.alaro <- FALSE
  is.arome <- FALSE
  if(!is.null(ex)){
    if(ex %in% c(tolower("alaro"),toupper("alaro"),"Alaro")){
      is.alaro = TRUE
    }
    if(ex %in% c(tolower("arome"),toupper("arome"),"Arome")){
      is.arome = TRUE
    }
  }

  if(is.alaro){
    ex <- extent(2.0, 2.0+508*0.05999999865889549, 55.11000097543001-446*0.03999999910593033,55.11000097543001)
  }
  if(is.arome){
    ex <- extent(5.484000, 22.11600, 42.972000, 51.828000)
  }



  library(TigR)
  library(raster)

  # executing wgrib to extract the date to a tempdir
  tmp <- tempfile(fileext = ".txt")

  if(!is.null(variable)){
    get.recnr <- paste(changeSlash(wgrib), changeSlash(gribfile), sep = " ")
    temp <- system(get.recnr, intern = TRUE)

    # query if variable is part of the grib file, if not return NULL
    if(length(grep(variable, temp)) == 0){
      warning(paste(variable, " not found in gribfile ", basename(gribfile), ". Returning NULL.", sep = ""))
      if(remove == "fail" | remove == TRUE){
        file.remove(gribfile)
      }
      return(NULL)
    }
    recnr <- as.numeric(strsplit(temp[grep(variable, temp)], ":", fixed = TRUE)[[1]][1])
  }

  wstring <- paste(changeSlash(wgrib), changeSlash(gribfile) ,"-d", recnr, "-text -o", tmp, sep = " ")
  system(wstring, show.output.on.console = FALSE)

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
    mat1[k,] <- suppressWarnings(as.numeric(IN[rows[k]:rows0[k],]))
  }

  # creating raster from matrix
  ras <- raster(mat1)

  if(!is.null(ex)){
    ras <- setExtent(ras, ex)
  }

  if(!is.null(crs)){
    crs(ras) <- crs
  }

  if(any(c(is.arome,is.alaro))){
    ras <- flip(ras, "y")
  }

  if(remove == "success" | remove == TRUE){
    file.remove(gribfile)
  }

  return(ras)

}
