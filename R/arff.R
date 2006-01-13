### Functions for reading and writing files in Weka ARFF format (see
### README and example data sets of version 3-4-6)

### <NOTE>
### String and evaluation types are enclosed by single quotes upon
### writing and enclosing single quotes are removed upon reading.
### </NOTE>

### (C) ceeboo 2005

read.arff <-
function(file)
{

    ## See read.table().
    if(is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if(!inherits(file, "connection")) 
        stop("argument 'file' must be a character string or connection")
    if(!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }

    ## Get header.
    attr <- NULL
    type <- NULL
    line <- readLines(file, n = 1)
    while(length(line) > 0 && 
          (regexpr('^[ \t]*@(?i)data', line,
                   perl = TRUE) == -1)) {
        if(regexpr('^[ \t]*@(?i)attribute', line,
                   perl = TRUE) > 0) {
            line <-
                unlist(strsplit(sub('^[ \t]+','',line),'[ \t]+'))
            if (length(line) < 3) 
                stop("invalid attribute specification")
            attr <- c(attr, sub("^'",'',sub("'$",'',line[2])))
            if ((l <- length(line)) > 3)
                line[3] <- paste(line[3:l],collapse="")
            type <- c(type,
                      sub(  'string','character',ignore.case=TRUE,
                          sub(   'real',  'numeric',ignore.case=TRUE,
                              sub( '\\{.+\\}','factor',ignore.case=TRUE,line[3]))))
        }
        line <- readLines(file, n=1)
    }
    
    ## Test header.
    if(length(line) == 0)
        stop("missing data section")
    if(is.null(attr))
        stop("missing attribute section")
    if(length(attr) !=
       length(grep('factor|numeric|character', type)))
        stop("invalid type specification")
    
    ## Get data.
    data <- read.table(file, sep=',', na.strings='?',
                       colClasses=type, comment.char='%')
    names(data) <- attr
    data
}

write.arff <-
function(x, file, eol = "\n")
{
    ## See write.table().
    if(file == "") 
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, 'w')
        on.exit(close(file))
    }
    if(!inherits(file, "connection")) 
        stop("argument 'file' must be a character string or connection")

    if (!is.data.frame(x) && !is.matrix(x))
        x <- data.frame(x)

    ## Weka expects a plain ? for NA but write.table() would quote it,
    ## so we single quote ourselves. 
    squote <- function(x)
        unlist(lapply(x, function(z) {
            if (is.na(z)) z
            else paste("'", z,"'", sep='')
        }))

    ## Write header.
    text <- paste('@relation', deparse(substitute(x)))
    writeLines(text, file, sep = eol)
    for(name in names(x)) {
        text <- paste('@attribute', name)
        if(is.factor(x[[name]])) {
            lev <- squote(levels(x[[name]]))
            levels(x[[name]]) <- lev
            text <- paste(text, " {",
                          paste(lev, collapse = ","), "}",
                          sep = "")
        } else if(is.character(x[[name]])) {
            text <- paste(text, "string")
            x[[name]] <- squote((x[[name]]))
        } else {
            text <- paste(text, "numeric")
        }
        writeLines(text, file, sep = eol)
    }

    ## Write data.
    writeLines("@data", file)
    write.table(x, file = file, na = "?", sep = ",",
                eol = eol, quote = FALSE, row.names = FALSE,
                col.names = FALSE)
}
