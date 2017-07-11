#' @title read.spartacus
#' @description Read information from ZAMG's spatacus dataset
#' @author Simon Frey
#' @param x either a single rda file or a vector of rda files containing spartacus data
#' @param crs a crs chracter string providing the spatial reference information of the spartacus file
#' @return a raster brick
#' @export

read.spartacus <- function(x, crs = "default"){

  library(raster)
  library(xts)

  rotate.matrix	<-	function(m) t(m)[,nrow(m):1]

  if(crs == "default"){
    crs = "+proj=lcc +lat_1=49 +lat_2=46 +lat_0=47.5 +lon_0=13.33333333333333 +x_0=400000 +y_0=400000 +ellps=GRS80 +units=m +no_defs"
  }

  # get month and year from x
  datevector	<-	unlist(lapply(x,FUN=function(x) strsplit(strsplit(x,".",fixed=TRUE)[[1]][1],"Tm",fixed=TRUE)[[1]][2]))
  datevector	<-	unlist(lapply(datevector,FUN=function(x) paste(substring(x,5),strtrim(x,4),sep="/")))

  out <- list()

  for(k in 1:length(x)){
    load(x[k]) # loaded variable is Tm

    X <- attr(Tm, "X")
    Y <- attr(Tm, "Y")

    for(m in 1:3) Tm <- rotate.matrix(Tm)

    # create raster from matrix
    out[[k]] <- raster(Tm/10,crs=crs,xmn=110855,xmx=695855,ymn=274198,ymx=588198)
  }
  names(out) <- datevector

  out <- brick(out)

  if(is.null(extractpoints)){
    return(out)
  }

}
