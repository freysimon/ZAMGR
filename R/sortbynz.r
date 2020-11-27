#' Sort by NZ
#' @description Sort an xts object by its header values (e.g. NZ)
#' @author Simon Frey
#' @details This function was designed to sort an xts object created by \code{\link{make.COSERO.input}}. It can read the xts object from file or it may be fed with one directly. If x an xts object, it must contain col.names with the nz information.
#' @seealso \link{make.COSERO.input}; \link{read.xts}
#' @param x xts object or character string to a file that will be read using \code{\link{read.xts}}.
#' @param writefile NULL or character string pointing to a file, that will be written.
#' @param returnmatrix logical. Should the result be returned to R?
#' @param ... additional parameters passed on to \code{\link{read.xts}}
#' @return An xts object, if returnmatrix == TRUE
#' @export
#' @import TigR
#'
#'

sortbynz <- function(x, writefile=NULL, returnmatrix=TRUE, ...){

  if(is.character(x)){
    nzext <- read.table(x,header=F,nrow=1)
    wp2 <- winProgressBar("Sorting the file", min = 0, max = length(nzext), label = paste("Reading input from file: ",x,sep=""),width=900L)

    IZMAT <- TigR::read.xts(x, ...)
    nzext <- colnames(IZMAT)


  } else {
    if(!is.xts(x)){
      stop("x must be an xts object or a character string pointing towards a file")
    }
    IZMAT <- x
    nzext <- colnames(x)
    wp2 <- winProgressBar("Sorting the file", min = 0, max = length(nzext), label = "",width=900L)
  }

  nzext <- as.numeric(nzext)

  # sort by NZ
  nz <- c(min(nzext):max(nzext))
  IZSORT <- IZMAT
  for(k in 1:ncol(IZSORT)){
    setWinProgressBar(wp2, label = paste("sorting... ", round(k/ncol(IZSORT)*100,2), "%", sep = " "), value = k)
    w <- which(colnames(IZMAT) == nz[k])
    cn <- nz[k]
    IZSORT[,k] <- IZMAT[,w]
  }
  IZMAT <- IZSORT
  colnames(IZMAT) <- cn

  rm(IZSORT)

  if(!is.null(writefile)){
    setWinProgressBar(wp2,label = paste("Writing sorted matrix to file: ",writefile,sep=""), value = k)

    TigR::write.xts(IZMAT, file = writefile, fmt="%6.2f", format="%Y %m %d %H %M", quote = FALSE, col.names = FALSE)
  }

  if(returnmatrix){
    return(IZMAT)
  }

  close(wp2)

}

