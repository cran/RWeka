Weka_control <-
function(...)
{
    rval <- list(...)
    if(length(rval) > 1 && is.null(names(rval)))
        stop("All arguments must be named.")
    class(rval) <- "Weka_control"
    return(rval)
}

print.Weka_control <-
function(x, ...)
{
    if(length(x) < 1) {
        writeLines(gettext("An empty list of Weka control options:"))
    } else {
        writeLines(gettextf("A list of Weka control options (%s):",
                            paste(as.character(x), collapse = " ")))
    }
    print.default(unclass(x))
    invisible(x)
}

as.character.Weka_control <-
function(x, ...)
{
    if(length(x) < 1) return(character(0))
  
    arg2char <- function(i) {
        arg <- paste("-", names(x)[i], sep = "")
        val <- x[[i]]
  
        rval <- if(is.logical(val)) {
            if(val) arg
        }  else {      
            c(arg, as.character(val))
        }
        return(as.vector(rval))
    }
  
    return(unlist(lapply(seq_along(x), arg2char)))
}

## Handlers.

make_Weka_control_handler <-
function(options, fun)
{
    ## Return a function which applies 'fun' to the values of the
    ## control arguments with names in options.
    ##
    ## This is useful as e.g. for meta learners, '-W' (for giving the
    ## base learner) requires the full Java class name, but R/Weka users
    ## do not necessarily (have to) know this.  Similarly, for savers,
    ## '-c' (for giving the class index) uses Java-style counting for
    ## its attributes, starting at 0.

    ## We handle both new-style 'control' arguments to R/Weka interface
    ## functions given via RWeka_control() and old-style specifications
    ## as character vectors.  For the former, the option values can be
    ## arbitrary R objects.

    ## This is not intended to validate the given control options.
    
    function(x) {
        if(inherits(x, "Weka_control")) {
            ## New-style Weka control lists:
            ##   Weka_control(W = value)
            ind <- which(names(x) %in% substring(options, 2))
            if(any(ind)) x[ind] <- lapply(x[ind], fun)
            ## And then coerce to character.
            x <- as.character(x)
        }
        else {
            ## Old-style character stuff:
            ##   c("-W", "J48")
            x <- as.character(x)        # Just making sure ...
            ind <- which(x %in% options)
            if(any(ind)) x[ind + 1] <- sapply(x[ind + 1], fun)
        }
        x
    }
}

