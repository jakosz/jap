dsh <-
function(scripts = NULL, plugins = c('ml', 'vi')) {
    data(dsh_scripts)
    savehistory()
    # No "clean" exit at the moment
    while(TRUE) {
        # Execute scripts in dev_shell environment
        if (!is.null(scripts)) {
            for (script in scripts) {
                try(source(script))
            }
        }
        # Read user input
        uInput = readline(prompt='dsh> ')
        write(uInput, '.Rhistory', append = TRUE)
        loadhistory()
        # Execute plugins on user input
        if (!is.null(plugins)) {
            for (plugin in plugins) {
                uInput = try(get(ps('dsh_', plugin))(uInput), silent = TRUE)
                if (class(uInput) == 'try-error') {
                    write(ps('Error in executing plugin ', plugin), stdout())
                    break
                }
            }
        }
        # Try to parse and evaluate user input
        res = try(eval(parse(text=uInput)), silent = TRUE)
        # Print necessary messages
        if (class(res) != 'try-error') {
            if (!is.null(res)) print(res)
        } else {
            write(ps('Error: ', attr(res, 'condition')$message), stdout())
        }
    }
}
