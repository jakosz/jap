# Wrapper for an R interactive session prompt; with every prompter iteration it:
# - reads a set of scripts, given as a character vector of paths;
#   so that scripts you're working on are loaded and syntactically verified on-the-fly
# - executes plugin functions on user input, so you can tweak its syntax
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

# XXX: Plugin functions' declarations should have a 'dsh_' prefix

# Simple multiline input handler
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

# Simple multiline input checker
# @dsh_ml
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

# Perform a system call to vim
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

