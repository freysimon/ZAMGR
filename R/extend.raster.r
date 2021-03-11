#' Extend a BIL raster file by NOVALUE data
#' @author Simon Frey
#' @description Extend an existing raster by NOVALUE data and opionally write it as a BIL raster file
#' @export
#' @import raster
#' @return a raster object if \code{write.raster == FALSE}
#' @param x a raster object
#' @param new.extent either an extent or a numerical vector that can be coerced to an extent.
#' @param write.raster logical. Should the extended raster be written to disk (TRUE) or returned (FALSE)?
#' @seealso \link{readINCABIL}
#'    \link{writeINCABIL}

extend.raster <- function(x, new.extent, write.raster = TRUE){

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

  x.extent <- raster::extent(x)
  x.resolution <- raster::res(x)

  x.new <- raster::raster()
  raster::crs(x.new) <- NA
  raster::extent(x.new) <- new.extent
  raster::res(x.new) <- x.resolution

  new.raster <- raster::merge(x.new,x)

  if(write.raster){
    writeINCABIL(x=new.raster,file="C:/TEMP/hyena/test.bil",hdr="update")
  } else {
    return(new.raster)
  }
}
