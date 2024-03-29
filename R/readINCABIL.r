
#' Read a binary INCA file
#' @description Read an INCA file in the BIL format from a .tar.gz archive.
#' @details Both uncompressed and compressed (.tar.gz) files can be processed. If uncompressed, only the filename without extension must be provided.#'
#' @author Simon Frey
#' @param filename A character string to the file to be read in
#' @param times  A character string giving the element of the BIL file to be read in. One of 'first', 'last', 'all', 'date'. See details.
#' @param date A POSIXct date or NULL. Only evaluated if times == date
#' @param remove Logical. Remove the processed files?
#' @param CoSys A character string giving the coordinate system of the raster or NULL.
#' @param tz A character string giving the time zone
#' @param form A character string. May be either 'list' or 'stack'
#' @param ex Either an extent, a numerical vector that can be coerced to an extent object or NULL.
#' @return A named (date and time) list with rasters if \code{form == 'list'}, or, if \code{form == 'stack'} a raster stack
#' @export
#' @import TigR
#' @import raster
#' @import sf
#' @import stringr
#' @examples
#'    # file to load
#'    file <- paste(path.package("ZAMGR"),"/extdata/INCA_TT.tar.gz",sep="")
#'
#'    # reading a compressed file
#'    x <- readINCABIL(file)
#'
#'    # plot the results
#'    plot(x[[1]])
#'
#'    # compress first and then read
#'    file <- untar(file, exdir = tempdir())
#'    file <- dir(path = tempdir(), pattern = ".bil", full.names = TRUE)
#'    x <- readINCABIL(file = gsub(".bil", replacement = "", file), times = "all")
#' @seealso \link{raster}
#'    \link{readBin}
#'    \link{proj4string}
#'    \link{crs}
#'    \link{writeINCABIL}
#'    \link{extent}

readINCABIL <- function(filename,times="first",date=NULL,remove=FALSE,CoSys = NULL, ex = NULL, tz = "utc", form = "stack"){

  #####################################################################
  #                                                                   #
  # Funktion um BIL Raster der ZAMG einzulesen                        #
  #                                                                   #
  # Die Dateien werden als BitStream gelesen und anschließend auf das #
  # INCA Raster aufgeprägt. Es können mehrere Blocks (Zeitschritte)   #
  # gelesen werden. (Jedoch nicht mehrere Dateien)                    #
  #                                                                   #
  # Über remove=TRUE (Standard) können die Rohdaten nach dem einlesen #
  # gelöscht werden.                                                  #
  #                                                                   #
  # Über CoSys kann der Rastern ein Koordinatensystem zugewiesen      #
  # werden (optional).                                                #
  #                                                                   #
  # Gibt eine Liste mit Rastern zurück                                #
  #                                                                   #
  # Autor: Simon Frey                                                 #
  # Version 0.1.9.0 (September 2016 - Juni 2022                       #
  #                                                                   #
  #####################################################################

  library(TigR)
  library(raster)


  # Überprüfen, ob für times ein sinnvolles Schlagwort angegeben wurde
  if(!times %in% c("first","last","all","date")){
    stop("times muss angegeben werden. Akzepiert werden first, last, all und date")
  }
  if(times == "date"){
    times <- "first"
    warning("times = date is deprecated and should not longer be used. Using times = first now.")
  }

  if(substrRight(filename,7) == ".tar.gz"){
    # make sure to purge tempdir first
    file.remove(dir(path = tempdir(), full.names = TRUE, pattern = "bil"))

    # Test if the archive is valide and if so, extract it. If it is not, return NULL

    valide <- TRUE

    tryCatch(untar(filename, exdir = tempdir(), tar = "internal", list = TRUE), error = function(e) {valide <<- FALSE })

    if(!valide){
      return(NULL)
    } else {
      untar(filename, exdir = tempdir(), tar = "internal")
    }

    # result <- tryCatch({
    #   expr = untar(filename, exdir = tempdir(), tar = "internal")
    #   },
    #   warning = function(war) {
    #     print("warning was generated")
    #     1
    #   },
    #   error = function(e){
    #     print("there was an error")
    #     NULL
    #   }
    # )


    filename <- dir(path = tempdir(), pattern = "bil", full.names = TRUE)
    filename <- gsub(".bil", replacement = "", filename)

  } else  if(substrRight(filename,4) %in% c(".tim",".bil",".hdr")){
    filename <- gsub(pattern=substrRight(filename,4),
                     replacement="", x=filename)
  }

  hdr <- paste(filename, ".hdr", sep="")
  tim <- paste(filename, ".tim", sep="")
  bil <- paste(filename, ".bil", sep="")

  warn <- FALSE

  if(any(!file.exists(c(hdr,tim,bil)))){
    warn = TRUE
  } else  if(any(file.size(c(hdr, tim, bil)) == 0)){
    warn = TRUE
  }

  if(warn){
    warning("Warning! Apparently one of the files does not exist or has size 0")
    return(NULL)
  }

  # Lese HDR File um Dimensionen zu berechnen
  hdr <- read.table(hdr,header = FALSE,stringsAsFactors = FALSE)
  nRows <- as.numeric(hdr[stringr::str_detect(hdr[,1],"(?i)nrows"),2])
  nCols <- as.numeric(hdr[stringr::str_detect(hdr[,1],"(?i)ncols"),2])
  nBlocks <- as.numeric(hdr[stringr::str_detect(hdr[,1],"(?i)nblocks"),2])
  ULX <- as.numeric(hdr[stringr::str_detect(hdr[,1],"(?i)ulxmap"),2])
  ULY <- as.numeric(hdr[stringr::str_detect(hdr[,1],"(?i)ulymap"),2])
  xdim <- as.numeric(hdr[stringr::str_detect(hdr[,1],"(?i)xdim"),2])
  ydim <- as.numeric(hdr[stringr::str_detect(hdr[,1],"(?i)ydim"),2])
  noData <- as.numeric(hdr[stringr::str_detect(hdr[,1],"(?i)nodata"),2])

  if(is.null(ex)){
    ext <- extent(ULX-xdim/2,
                ULX+nCols*xdim-xdim/2,
                ULY-nRows*ydim+ydim/2,
                ULY+ydim/2)
  } else if(is.numeric(ex)){
    ext <- extent(ex)
  } else {
    stop("ERROR: ex must be an extent object or a numeric vector that can be coerced to one.")
  }


  datenotfound = FALSE

  if(file.info(tim)$size > 0){

    # Lese TIM file um die gespeicherten Zeitpunkte zu identifizieren
    tim <- read.table(tim, header=FALSE,colClasses=c("character","numeric"))
    timesteps <- as.POSIXct(tim[,1],format="%Y%m%d%H%M", tz = tz)

    # Lese bil file als bitsream
    bil <- readBin(bil,what="integer",size=2,
                   n=nRows*nCols*nBlocks)

    # Daten durch 10 teilen (aufgrund der ZAMG)
    bil[bil != noData] <- bil[bil != noData]/10

    # Auswertung von times und demensprechende Umsetzung
    if(times == "first"){
      startBlocks <- 1
      endBlocks <- 1
      k_init = 1
    }
    if(times == "last"){
      startBlocks <- nBlocks
      endBlocks <- nBlocks
      k_init = nRows*nCols*(nBlocks-1) + 1
    }
    if(times == "all"){
      startBlocks <- 1
      endBlocks <- nBlocks
      k_init = 1
    }
    # if(times == "date"){
    #   if(any(timesteps == date, na.rm = TRUE)){
    #     startBlocks <- min(which(timesteps == date))
    #     endBlocks <- max(which(timesteps == date))
    #   } else {
    #     datenotfound = TRUE
    #     startBlocks <- 1
    #     endBlocks <- 1
    #   }
    # }
  } else {
    datenotfound = TRUE
  }

  # Liste allokieren
  LIST <- vector("list",max(1,endBlocks-startBlocks+1))
  OUTRASTER <- LIST
  for(i in 1:max(1,endBlocks-startBlocks+1)){
    LIST[[i]] <- matrix(nrow=nRows,ncol=nCols,data=NA)
  }

  if(!datenotfound){
     # Daten auf Raster prägen
    k = k_init
    l = 1
    for(b in startBlocks:endBlocks){
      for(i in 1:nRows){
        for(j in 1:nCols){
          LIST[[l]][i,j] <- bil[k]
          k = k + 1
        }
      }
      l = l + 1
    }
  } else {
    # Daten auf Raster prägen
    k = k_init
    l = 1
    for(b in startBlocks:endBlocks){
      for(i in 1:nRows){
        for(j in 1:nCols){
          LIST[[l]][i,j] <- -999
          k = k + 1
        }
      }
      l = l + 1
    }
  }



  for(i in 1:max(1,endBlocks-startBlocks+1)){
    OUTRASTER[[i]] <- raster(LIST[[i]])
    extent(OUTRASTER[[i]]) <- ext
  }

  if(remove){
    file.remove(paste(filename, ".hdr", sep=""))
    file.remove(paste(filename, ".bil", sep=""))
    file.remove(paste(filename, ".tim", sep=""))
  }

  # Datum als Rasternamen schreiben
  if(times == "first"){
    names(OUTRASTER) <- format(timesteps[1],format="%Y-%m-%d %H:%M")
  } else if(times == "last"){
    names(OUTRASTER) <- format(timesteps[length(timesteps)],format="%Y-%m-%d %H:%M")
  } else if(times == "all"){
    names(OUTRASTER) <- format(timesteps,format="%Y-%m-%d %H:%M")
  } # else {
  #   names(OUTRASTER) <- format(date,format="%Y-%m-%d %H:%M")
  # }


  # Koordinatensystem zuweisen, falls angegeben
  if(!is.null(CoSys)){
    for(k in 1:length(OUTRASTER)){
      terra::crs(OUTRASTER[[k]]) <- CoSys
    }
  }

  if(form == "stack"){
    OUTRASTER <- stack(OUTRASTER)
  }


  return(OUTRASTER)
}



