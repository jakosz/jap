ht <-
function(x, n = 6) {
    if (class(x) %in% c('matrix', 'data.frame')) {
        lFun = nrow
        lNam = 'rows'
    } else {
        lFun = length
        lNam = 'elements'
    }
    print(head(x, n))
    writeLines(ps('\n.. [ ', lFun(x), ' ', lNam, ' total ] ..\n'))
    print(tail(x, n))
}
