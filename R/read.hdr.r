#' Read an HDR file
#' @description Read an HDR file which is the header part of a BIL raster file
#' @author Simon Frey
#' @export
#' @import stringr
#' @return a data.frame containing the arguments of the hdr file
#' @param file character string. Filename of the file to be read.
#' @param add.nblocks logical. Should the agrument nblocks be added to the output? See details.
#' @details If add.nblocks is TRUE (the default) then the argument \code{nblocks} is added to the output list (if it does nikt already exist).
#'     If so, the value from the argument \code{nbands} is copied to \code{nblocks} and \code{nbands} is set to 1. This is because of handling
#'     in Delft-FEWS. See the wiki of Delft-FEWS: (https://publicwiki.deltares.nl/display/FEWSDOC/BIL+BIP+BSQ+Parser).
#'

read.hdr <- function(file, add.nblocks = TRUE){

  hdr <- scan(file = file, what = "character", blank.lines.skip = F)

  # Determine the arguments section of the file
  end.of.args <- which(hdr == "")[1]
  if(!is.na(end.of.args)){ # will be NA if no blank line is present in the file
    nlines <- floor(end.of.args/2)
  } else {
    nlines <- floor(length(hdr))
  }

  hdr <- read.table(file, header=F, nrows = nlines, colClasses = "character", stringsAsFactors = FALSE)

  if(add.nblocks){
    # check if nblocks already exists
    nblocks.exists <- any(stringr::str_detect(hdr[,1],"(?i)nblocks"))# ?i makes the regex case insensitive
    if(!nblocks.exists){
      nbands <- which(stringr::str_detect(hdr[,1],"(?i)nbands"))# ?i makes the regex case insensitive
      hdr <- rbind(hdr, c(ifelse(toupper(hdr[1,1]) == hdr[1,1],"NBLOCKS","nblocks"), hdr[nbands,2]))
      hdr[nbands,2] <- 1
    }
  }

  return(hdr)
}
