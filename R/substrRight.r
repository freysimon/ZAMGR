# Letzte n Buchstaben eines Characters auswerten
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
