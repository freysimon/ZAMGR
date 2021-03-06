#' Write a raster using the BIL format
#' @description Write a raster in the BIL format used e.g. by INCA
#' @author Simon Frey
#' @export
#' @import TigR
#' @import raster
#' @import tools
#' @param x raster object. The raster that will be written.
#' @param file character string. The file that will be written. Note that besides the .bil file, a .hdr and a .stx file are written, too.
#' @param delete.stx logical. Should the stx file be deleted?
#' @param overwrite logical. Should an existing file be overwritten?
#' @param hdr character string. Any of "keep", "delete", or "update".
#' @param bandorder any of 'BIL', 'BSQ', or 'BIP'
#' @param noValue numeric or NULL. What value is assigned to nodata?
#' @seealso \link{readINCABIL}
#'    \link{readZAMGGRIB}
#'    \link{read.hdr}
#' @details \code{file} must be the filename of a raster file thar will be written. It may or may not have the file extension .bil. If not, it will be added to the srtring.
#'
#'      The \code{stx}-file is not necessary for further usage of the raster file and may be delted using \code{delete.stx == TRUE}.
#'
#'      The \code{hdr}-file matches the official ESRI specifications. However, Delft-FEWS needs the parameter \code{NBLOCKS} for reading time series. If \code{hdr == 'update'}
#'      the parameter \code{NBANDS} will be set to 1 and the new added parameter \code{NBLOCKS} will receive the former value of \code{NBANDS}.
#'
#'      \code{hdr == 'delete'} deltes the \code{hdr}-file whereas \code{hdr == 'keep'} keeps it unedited.
#'
#'      \code{bandorder} sets the bandorder of the binary raster file.



writeINCABIL <- function(x, file, delete.stx=TRUE, overwrite = TRUE, hdr = "keep", bandorder='BIL', noValue = NULL){

  if(!hdr %in% c("keep","delete","update")){
    stop("ERROR: hdr must be one of 'keep', 'delete', or 'update'.")
  }

  if(!bandorder %in% c("BIL","BSQ","BIP")){
    stop("ERROR: bandorder must be any of 'BIL', 'BSQ', or 'BIP'")
  }

  ex <- tools::file_ext(file)
  if(ex != "bil"){
    if(ex == ""){
      file <- paste(file,".bil",sep="")
    } else {
      stop("ERROR: File must be a valid filename")
    }
  }

  bandorder <- tolower(bandorder)

  # temporary change the fileextension to bandorder value
  file <- gsub("bil",bandorder,file)

  writeRaster(x, filename = file, overwrite = overwrite, bandorder = bandorder, datatype='INT2S')

  # change file extension back to "bil"
  file.rename(from=file, to = gsub(bandorder,"bil",file))
  file <- gsub(bandorder,"bil",file)


  if(delete.stx){
    file.remove(gsub(".bil",".stx",file,fixed=TRUE))
  }

  if(hdr == "delete"){
    file.remove(gsub(".bil",".hdr",file,fixed=TRUE))
  }
  if(hdr == "update"){
    hdr.new  <- read.hdr(file = gsub(".bil",".hdr", file, fixed = TRUE), add.nblocks = TRUE, bandorder = "BIL", noValue = noValue)
    write.table(x=hdr.new, file = gsub(".bil",".hdr", file, fixed = TRUE),
                quote = FALSE, col.names = FALSE, row.names = FALSE, sep = " ")
  }

}
