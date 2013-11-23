dsh_ml <-
function(uInput) {
    savehistory()
    while(is.ml(uInput)) {
        nInput = readline(prompt='dsh+ ')
        write(nInput, '.Rhistory', append = TRUE)
        loadhistory()
        uInput = ps(uInput, nInput, sep = '\n')
    }
    return(uInput)
}
