# ------------------------------------------------------------------------------ alias

len = function(x) {
    return( length(x) )
}

ps = function(..., sep = '') {
    return(paste(..., sep=sep))
}

chr = function(x) {
    return( as.character(x) )
}

nmr <-
function(x) {
    return( as.numeric(as.character(x)) )
}

lu = function(x) {
    return( length(unique(x)) )
}

vs = function(x, sep = '') {
    return( paste(x, collapse = sep) )
}

lw = function(...) {
    return( length(which(...)) )
}

s = function(...) {
    return( system(ps(...)) )
}

splt <-
function(x, split = '', ...) {
    return(unlist(strsplit(x, split = split, ...)))
}

sr <-
function(x, ...) {
    scale_to_range(x, ...)
}

exe <-
function(x) {
    return(eval(parse(text=x)))
}

# ------------------------------------------------------------------------------ misc

ht <- # head & tail
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

# seq_len from first to last-1 element
seq_le <-
function(x) {
    return(seq_len(length(x)-1))
}

# seq_len from second to last element
seq_en <-
function(x) {
    return(seq_len((length(x)-1))+1)
}

# If possible, converts a vector of any class to numeric; if not, to character
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

# Checks which elements in a character vector are (all) uppercase/lowercase:
is.upper <-
function(x) {
    return(sapply(x, function(s) all(sapply(splt(s), function(l) l == toupper(l)))))
}

is.lower <-
function(x) {
    return(sapply(x, function(s) all(sapply(splt(s), function(l) l == tolower(l)))))
}

# From each string in a vector return just numbers, 
# preserving their class (character) and order of appearance:
extract_numbers <-
function(x) {
    self_from_string <-
    function(x) {
        sapply(splt(x), function(x) if (!is.na(as.numeric(x))) x)
    }
    return(sapply(x, self_from_string))
}

# Checks whether x starts (from = 's') or ends (from = 'e') with a given pattern
# @%startstwith% @%endswith%
swew <-
function(x, pattern, from) {
    if (length(x) != 1) {
        stop('x must be of length 1')
    }
    return(any(unlist(lapply(c(1:nchar(x)), 
                             function(i) {
                                 if (from == 's') Str = substr(x, 1, i) 
                                 if (from == 'e') Str = substr(x, i, nchar(x))
                                 return(Str == pattern)
                             })))
    )
}

# Checks whether a string x starts with any of the given patterns 
'%startswith%' <-
function(x, pattern) {
    return(any(unlist(lapply(x, swew, pattern, 's'))))
}

# Checks whether a string x ends with any of the given patterns
'%endswith%' <-
function(x, pattern) {
    return(any(unlist(lapply(x, swew, pattern, 'e'))))
}

# grep with logical output, for use in conditioning;
# no match in grep gives integer(0) which yields error when used in if()
greps <-
function(pattern, x, fixed = FALSE) {
    if (length(grep(pattern, x, fixed = fixed)) != 0) {
        return(1)
    } else {
        return(0)
    }
}

# Scale a numeric vector and transform it so it has min at 'from' and max at 'to'
scale_to_range <-
function(x, from = 1, to = 100) {
    # skalowanie:
    x = scale(x)[,1]
    # przesunięcie: wartość min. wyrównać do 0:
    x = x+abs(min(x))
    # przesunięcie: wartość max. wyrównać do (to - from):
    x = x*((to-from)/max(x))
    # przesunięcie o wartość from;
    return(x+from)
}

# Returns a random[, case-sensitive] alphanumeric string of length n
rx <-
function(n=16) { 
    return(paste(sample(c(letters, LETTERS, 0:9), n, replace = TRUE), collapse = ''))
}

# Group indices of a list or vector into n (almost) equal chunks:
# @mc.rbind
iTable <-
function(x, n) {
    x = seq_along(x)
    i = floor(seq(from = x[1], to = x[length(x)], length.out = n+1))
    res = data.frame(from = i[1:(length(i)-1)], to = i[2:length(i)])
    res$from[2:nrow(res)] = res$from[2:nrow(res)] + 1
    return(res)
}

# Group indices of a list or vector into chunks of length n:
xTable <-
function(x, n) {
    from = seq(from = 1, to = len(x), by = n)
    to = c(from[2:len(from)] - 1, len(x))
    return(data.frame(from = from, to = to))
}

# rbind a list of data frames (ldf), 
# splitting the job to all cores detected by detectCores()
mc.rbind <-
function(ldf) {
    itable = iTable(ldf, detectCores())
    res = mclapply(c(1:detectCores()), 
                   function(x) {
                       do.call('rbind', ldf[itable$from[x]:itable$to[x]])
                   })
    res = do.call('rbind', res)
    return(res)
}

dsh <-
function(scripts = NULL, plugins = c('ml', 'vi')) {
    data(dsh_scripts)
    savehistory()
    # No "clean" exit at the moment
    while(TRUE) {
        # Read user input
        uInput = readline(prompt='dsh> ')
        write(uInput, '.Rhistory', append = TRUE)
        loadhistory()
        # Execute scripts in dev_shell environment
        if (!is.null(scripts)) {
            for (script in scripts) {
                try(source(script))
            }
        }
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

# Simple multiline input handler (dsh plugin)
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

# Perform a system call to vim (dsh plugin)
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

plot_empty <-
function(xlim=c(-1,1),ylim=c(-1,1),bty='n',xaxt='n',yaxt='n',xlab='',ylab='',
         type='n',layout = NULL,...) {
    if (!is.null(layout)) {
        xlim = c(min(layout[,1]), max(layout[,1]))
        ylim = c(min(layout[,2]), max(layout[,2]))
    }
    plot(0,xlim=xlim,ylim=ylim,bty=bty,xaxt=xaxt,yaxt=yaxt,xlab=xlab,ylab=ylab,
         type=type,layout=layout,...)
}

# konwersja nazw kolumn na indeksy:
n2i <-
function(df, n) {
    out = vector()
    for (name in n) {
        out = c(out, which(names(df) == name))
    }
    return(out)
}
# konwersja indeksów kolumn na nazwy:
i2n <-
function(df, i) {
    out = vector()
    for (index in i) {
        out = c(out, names(df)[index])
    }
    return(out)
}

# zwróć indeksy lub nazwy (wedle życzenia) kolumn, niezależnie od tego
# co dostałeś na wejściu:
ni <-
function(df,        
         Vector,    # wektor indeksów kolumn lub nazw kolumn
         what) {    # co ma być wynikiem? 'i' (indeksy) czy 'n' (nazwy)?
    if (what == 'i') {
        if (is.numeric(Vector)) {
            return(Vector)
        } else {
            return(n2i(df, Vector))
        }
    }
    if (what == 'n') {
        if(is.numeric(Vector)) {
            return(i2n(df, Vector))
        } else {
            return(Vector)
        }
    }
}
