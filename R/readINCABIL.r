

readINCABIL <- function(filename,times="first",date=NULL,remove=TRUE,CoSys = NULL, tz = "utc"){

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
  # Version 0.4 (September 2016 - Jänner 2017)                        #
  #                                                                   #
  #####################################################################


  # Überprüfen, ob für times ein sinnvolles Schlagwort angegeben wurde
  if(!times %in% c("first","last","all","date")){
    stop("times muss angegeben werden. Akzepiert werden first, last, all und date")
  }
  if(times == "date"){
    if(is.null(date)){
      stop("Wenn times = date muss date angegeben werden")
    } else if(!"POSIXct" %in% class(date)){
      stop("date muss als POSIXct angegeben werden")
    }
  }

  if(substrRight(filename,7) == ".tar.gz"){
    # make sure to purge tempdir first
    file.remove(dir(path = tempdir(), full.names = TRUE, pattern = "bil"))
    untar(filename, exdir = tempdir())
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
  nRows <- as.numeric(hdr[hdr[,1]=="nRows",2])
  nCols <- as.numeric(hdr[hdr[,1]=="nCols",2])
  nBlocks <- as.numeric(hdr[hdr[,1]=="nBlocks",2])
  ULX <- as.numeric(hdr[hdr[,1]=="ULXmap",2])
  ULY <- as.numeric(hdr[hdr[,1]=="ULYmap",2])
  xdim <- as.numeric(hdr[hdr[,1]=="Xdim",2])
  ydim <- as.numeric(hdr[hdr[,1]=="Ydim",2])
  noData <- as.numeric(hdr[hdr[,1]=="NoData",2])

  ext <- extent(ULX-xdim/2,
                ULX+nCols*1000-xdim/2,
                ULY-nRows*1000+ydim/2,
                ULY+ydim/2)

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
    }
    if(times == "last"){
      startBlocks <- nBlocks
      endBlocks <- nBlocks
    }
    if(times == "all"){
      startBlocks <- 1
      endBlocks <- nBlocks
    }
    if(times == "date"){
      if(any(timesteps == date, na.rm = TRUE)){
        startBlocks <- min(which(timesteps == date))
        endBlocks <- max(which(timesteps == date))
      } else {
        datenotfound = TRUE
        startBlocks <- 1
        endBlocks <- 1
      }
    }
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
    k = 1
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
    k = 1
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
  names(OUTRASTER) <- format(timesteps[1],format="%Y-%m-%d %H:%M")

  # Koordinatensystem zuweisen, falls angegeben
  if(!is.null(CoSys)){
    for(k in 1:length(OUTRASTER)){
      crs(OUTRASTER[[k]]) <- CoSys
    }
  }

  return(OUTRASTER)
}



