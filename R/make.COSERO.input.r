#' Create a T_IZ or P_IZ file from a set of INCA-Files
#' @param f A character vector of INCA files
#' @param shape Absolute path to a shapefile (points) at which locations the values of the INCA files are extracted
#' @param nzraster Path to a raster with the nz information
#' @param output name of the outputfile or NULL
#' @param ... parameters passed to readINCABIL
#' @return if output == NULL the IZ-matrix is returned. Else the IZ-File is written and nothing is returned to R.
#' @author Simon Frey
#' @export
#' @seealso \link{readINCABIL}
make.COSERO.input <- function(f, shape, nzraster, output = NULL, ...){
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

  # make matrix with ncol = nzext + 1 for the date and nrow = length(INCAFiles)
  IZMAT <- as.data.frame(matrix(ncol = length(nzext)+1, nrow = length(INCAFiles), data = NA))
  colnames(IZMAT) <- c("Date", nzext)

  wp <- winProgressBar("Processing INCA Files", min = 0, label = "starting...", max = length(INCAFiles))

  # process INCAFiles
  for(k in 1:length(INCAFiles)){
    setWinProgressBar(wp, label = INCAFiles[k], value=k)
    file <- readINCABIL(INCAFiles[k], CoSys = p4j, ...)
    if(is.null(file)){
      next
    }
    date <- last(unlist(strsplit(names(file), ".", fixed = TRUE)))
    ext <- extract(file[[1]], shape)
    IZMAT[k,1] <- date
    IZMAT[k,2:ncol(IZMAT)] <- ext
  }

  close(wp)

  if(is.null(output)){
    return(IZMAT)
  } else {
    write.table(IZMAT, file = output, col.names=TRUE, row.names=FALSE, sep="\t", quote = FALSE)
  }
}
