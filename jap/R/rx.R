rx <-
function(n=16) { 
    return(paste(sample(c(letters, LETTERS, 0:9), n, replace = TRUE), collapse = ''))
}
