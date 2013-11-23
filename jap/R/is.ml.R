is.ml <-
function(uInput) {
    ui = splt(uInput, '')
    # Check for open parentheses:
    tests = lapply(list(c('\\(', '\\)'), c('\\{', '\\}')), 
                   function(x) length(grep(x[1], ui)) > length(grep(x[2], ui)))
    # Check for special line endings
    eols = c('+', '-', '*', '/', '^', '=')
    tests = c(tests, gsub(' ', '', uInput) %endswith% eols)
    # Check for open quotation:
    tests = c(tests, lapply(c('"', "'"), function(x) length(grep(x, ui)) %% 2 != 0))
    return(any(unlist(tests)))
}
