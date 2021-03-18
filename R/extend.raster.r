#' Extend a BIL raster file by NOVALUE data
#' @author Simon Frey
#' @description Extend an existing raster by NOVALUE data and opionally write it as a BIL raster file
#' @export
#' @import raster
#' @return a raster object if \code{write.raster == FALSE}
#' @param x a raster object. May be a raster, a RasterBrick or a RasterStack.
#' @param new.extent either an extent or a numerical vector that can be coerced to an extent.
#' @param write.raster logical. Should the extended raster be written to disk (TRUE) or returned (FALSE)?
#' @param filename character string. Filename used to write the extended raster. Only used if write.raster==TRUE.
#' @param mp numeric. multiplicator to multiply the (existing) raster with.
#' @param noValue numeric. Sets the nodata value for the new raster.
#' @seealso \link{readINCABIL}
#'    \link{writeINCABIL}

extend.raster <- function(x, new.extent, write.raster = TRUE, filename=NULL, mp = 1, noValue = NULL){

  library(raster)

  if(!class(x) %in% c("RasterLayer","RasterStack", "RasterBrick")){
    stop("ERROR: x must be a raster object")
  }

  if(class(new.extent) != "Extent"){
    if(is.vector(new.extent)){
      new.extent <- raster::extent(new.extent)
    }
  } else {
    stop("ERROR: new.extent must be an extent or a numerical vector that can be coerced to an extent")
  }

  x <- x*mp

  x.extent <- raster::extent(x)
  x.resolution <- raster::res(x)

  x.new <- raster::raster()
  raster::crs(x.new) <- NA
  raster::extent(x.new) <- new.extent
  raster::res(x.new) <- x.resolution

  new.raster <- raster::merge(x.new,x)

  if(!is.null(noValue)){
    new.raster[is.na(new.raster)] <- noValue
    NAvalue(new.raster) <- noValue
  }




  if(write.raster){
    writeINCABIL(x=new.raster,file=filename,hdr="update")
  } else {
    return(new.raster)
  }
}
