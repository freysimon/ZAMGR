#' Create a T_IZ or P_IZ file from a set of INCA-Files
#' @param f A character vector of INCA files
#' @param shape Absolute path to a shapefile (points) at which locations the values of the INCA files are extracted
#' @param nzraster Path to a raster with the nz information
#' @param output name of the outputfile or NULL
#' @param otf logical. write on-the-fly (don't hold the whole matrix in memory)
#' @param sortbynz logical. Should the output be sorted according to the nz information?
#' @param ... parameters passed to readINCABIL
#' @return if output == NULL the IZ-matrix is returned. Else the IZ-File is written and nothing is returned to R.
#' @author Simon Frey
#' @export
#' @seealso \link{readINCABIL}
#' @import rgdal
#' @import raster
#' @import TigR
#' @import xts
#' @details Reads a set of INCA files (Precipitation or Temperature) and calculates a matrix that can be read by COSERO.
#'
#'     The shapefile must be projected in the same coordinate systm as the nzraster as well as the INCA files.
#'     Normally this is "+proj=lcc +lat_1=46 +lat_2=49 +lat_0=47.5 +lon_0=13.33333333333333 +x_0=400000 +y_0=400000 +ellps=bessel +units=m +no_defs"
#'
make.COSERO.input <- function(f, shape, nzraster, output = NULL, otf = FALSE, sortbynz = TRUE, ...){
  library(rgdal)
  library(raster)
  library(TigR)
  library(xts)



  # read shapefile
  shp <- gsub(".shp","",last(unlist(strsplit(shape, "/", fixed = TRUE))))
  dsn <- unlist(strsplit(shape, "/", fixed = TRUE))
  dsn <- paste(dsn[1:(length(dsn)-1)], collapse = "/")
  shape <- readOGR(dsn = dsn, layer = shp)
  p4j <- proj4string(shape)

  # read nzraster
  nzras <- raster(nzraster)


  # extract nz values to get right information about the spatial location
  nzext <- extract(nzras, shape)

  # make matrix with ncol = nzext + 1 for the date and nrow = length(f)
  if(!otf){
     IZMAT <- as.data.frame(matrix(ncol = length(nzext)+1, nrow = length(f), data = NA))
     colnames(IZMAT) <- c("Date", nzext)
  } else {
    if(is.null(output)){
      stop("Output must not be NULL if otf is TRUE")
    } else {
      wt <- c("Date", nzext)
      write.table(t(wt), file = output, col.names = FALSE, row.names = FALSE,
                  sep="\t", quote = FALSE)
    }
  }


  wp <- winProgressBar("Processing INCA Files", min = 0, label = "starting...", max = length(f))

  # process f
  for(k in 1:length(f)){
    setWinProgressBar(wp, label = f[k], value=k)
    file <- readINCABIL(f[k], CoSys = p4j, ...)
    if(is.null(file)){
      next
    }
    date <- last(unlist(strsplit(names(file), ".", fixed = TRUE)))
    ext <- extract(file[[1]], shape)
    if(otf){
      ext <- c(date, ext)
      write.table(t(ext), file = output, append = TRUE, quote = FALSE,
                  col.names = FALSE, row.names = FALSE, sep = "\t")
    } else {
      IZMAT[k,1] <- date
      IZMAT[k,2:ncol(IZMAT)] <- ext
    }

  }

  close(wp)

  if(sortbynz){

    wp <- winProgressBar("Sorting the file", min = 0, max = length(nzext), label = "")

    if(otf){
      IZMAT <- read.table(file = output, header = FALSE, skip = 1, sep = "\t",
                          colClasses = c("character", rep("numeric", length(nzext))))
      colnames(IZMAT) <- c("Date", nzext)
    }
    # sort by NZ
    nz <- c(min(nzext):max(nzext))
    IZSORT <- IZMAT[,2:ncol(IZMAT)]
    for(k in 1:ncol(IZSORT)){
      setWinProgressBar(wp, label = paste(round(k/ncol(IZSORT),2), "%", sep = " "), value = k)
      w <- which(colnames(IZMAT) == nz[k])
      IZSORT[,k] <- IZMAT[,w]
    }
    IZMAT <- cbind(IZMAT[,1], IZSORT)
    colnames(IZMAT) <- c("Date",nz)

    rm(IZSORT)

    if(otf){
      write.table(IZMAT, file = output, col.names=TRUE, row.names=FALSE, sep="\t", quote = FALSE)
    }

    close(Wp)

  }

  if(is.null(output)){
    return(IZMAT)
  } else if(!otf){
    write.table(IZMAT, file = output, col.names=TRUE, row.names=FALSE, sep="\t", quote = FALSE)
  }
}
