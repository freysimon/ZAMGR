#' Create a T_IZ or P_IZ file from a set of INCA-Files
#' @param f A character vector of INCA files
#' @param shape Absolute path to a shapefile (points) at which locations the values of the INCA files are extracted
#' @param nzraster Path to a raster with the nz information
#' @param output name of the outputfile or NULL
#' @param otf logical. write on-the-fly (don't hold the whole matrix in memory)
#' @param sortbynz logical. Should the output be sorted according to the nz information?
#' @param fillmissing logical. Should missing time steps be filled in the output? See details.
#' @param ... parameters passed to readINCABIL
#' @return if output == NULL the IZ-matrix is returned. Else the IZ-File is written and the IZ-matrix is returned.
#' @author Simon Frey
#' @export
#' @seealso \link{readINCABIL}, \link{fill.missing}
#' @import terra
#' @import raster
#' @import TigR
#' @import xts
#' @details Reads a set of INCA files (Precipitation or Temperature) and calculates a matrix that can be read by COSERO.
#'
#'     The shapefile must be projected in the same coordinate system as the nzraster as well as the INCA files.
#'     Normally this is "+proj=lcc +lat_1=46 +lat_2=49 +lat_0=47.5 +lon_0=13.33333333333333 +x_0=400000 +y_0=400000 +ellps=bessel +units=m +no_defs"
#'
#'     \code{fillmissing}: If fillmissing == TRUE the output will be checked on missing timesteps and - if any - will be filled using \code{\link{fill.missing}}.
#'     Note that if otf == TRUE, the filled matrix will be written to a copy of the otf-output
#'
make.COSERO.input <- function(f, shape, nzraster, output = NULL, otf = FALSE,
                              sortbynz = TRUE, fillmissing = TRUE, ...){
  library(rgdal)
  library(raster)
  library(TigR)
  library(xts)



  # read shapefile
  shape <- TigR::readogr(shape)
  p4j <- terra::crs(shape)

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

      IZMAT <- as.data.frame(matrix(ncol = length(nzext)+1, nrow = length(f), data = NA))
      colnames(IZMAT) <- c("Date", nzext)
    }
  }


  wp <- winProgressBar("Processing INCA Files", min = 0, label = "starting...", max = length(f), width = 500L)

  # process f
  for(k in 1:length(f)){
    setWinProgressBar(wp, label = f[k], value=k)
    file <- readINCABIL(f[k], CoSys = p4j, ...)
    if(is.null(file)){
      next
    }
    date <- as.character(
      format(
        as.POSIXct(names(file), format = "X%Y.%m.%d.%H.%M", tz = "UTC"),
        format = "%Y %m %d %H %M")
    )

    #date <- last(unlist(strsplit(names(file), ".", fixed = TRUE)))
    ext <- extract(file[[1]], shape)
    ext <- c(date, ext)
    IZMAT[k,] <- ext
    if(otf){
      write.table(t(ext), file = output, append = TRUE, quote = FALSE,
                  col.names = FALSE, row.names = FALSE, sep = "\t")
    }
  }

  close(wp)

  if(sortbynz){
    IZMAT <- sortbynz(x=IZMAT, writefile=FALSE, returnmatrix=TRUE, tz = "utc")
  }

  if(fillmissing){
    CN <- colnames(IZMAT)
    IZMAT <- fill.missing(xts(IZMAT[,2:ncol(IZMAT)],
                              order.by = as.POSIXct(IZMAT[,1], format = "%Y %m %d %H %M", tz = "UTC")))
    tmp <- as.data.frame(IZMAT)
    IZMAT <- cbind(as.character(format(index(IZMAT), format = "%Y %m %d %H %M")), tmp)
    colnames(IZMAT) <- CN
    rm(tmp)
    rm(CN)
  }

  if(!is.null(output)){
    if(!otf){
      write.table(IZMAT, file = output, col.names=TRUE, row.names=FALSE, sep="\t", quote = FALSE)
    }
  }
  if(otf){
    write.table(IZMAT, file = paste(dirname(output),"/Filled_Copy_of_",basename(output),sep=""), col.names=TRUE, row.names=FALSE, sep="\t", quote = FALSE)
  }

  return(IZMAT)

}
