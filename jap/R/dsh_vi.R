dsh_vi <-
function(uInput) {
    if (uInput %in% c('vi', 'vim')) {
        s('vim')
    } else if (uInput == 'vis') {
        s('vim ', get('scripts', envir = parent.frame())[1])
    } else {
        return(uInput)
    }
}
