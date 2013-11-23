as.numchar <-
function(x) {
    return(unlist(lapply(x, function(i) {  
                  if (is.na(nmr(i))) {
                      return(chr(i))
                  } else {
                      return(nmr(i))
                  }
              })))
}
