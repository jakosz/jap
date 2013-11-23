extract_numbers <-
function(x) {
    self_from_string <-
    function(x) {
        sapply(splt(x), function(x) if (!is.na(as.numeric(x))) x)
    }
    return(sapply(x, self_from_string))
}
