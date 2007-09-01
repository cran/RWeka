Weka_control <-
function(...)
{
    rval <- list(...)
    ## <FIXME>
    ## Use nzchar() when 2.6.0 is required.
    if((length(rval) > 0)
       && (is.null(names(rval)) || !all(nchar(names(rval)) > 0)))
        stop("All arguments must be named.")
    ## </FIXME>
    structure(rval, class = "Weka_control")
}

print.Weka_control <-
function(x, ...)
{
    if(length(x) < 1) {
        writeLines(gettext("An empty list of Weka control options."))
    } else {
        writeLines(gettextf("A list of Weka control options (%s):\n",
                            paste(as.character(x), collapse = " ")))
        ## (Note that this is quite perfect in case of control lists
        ## with recursive elements.)
        print.default(unclass(x))
    }
    invisible(x)
}

as.character.Weka_control <-
function(x, ...)
{
    if(!length(x)) return(character(0))

    ## <FIXME>
    ## Use base::Map() when depending on R >= 2.6.0, but note that
    ## (currently?) this has USE.NAMES = TRUE ...
    map <- function(f, ...)
        mapply(f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
    ## </FIXME>

    arg2char <- function(tag, val) {
        ## Works for a single (possibly empty) tag and a single value,
        ## which can be recursive, so that we can handle things like
        ##   Weka_control(K = list("RBFKernel", G = 2))
        ## <FIXME>
        ## Use nzchar() when 2.6.0 is required.
        tag <- if((nchar(tag) > 0)) paste("-", tag, sep = "") else NULL
        ## </FIXME>
        out <- if(is.list(val)) {
            nms <- names(val)
            if(is.null(nms)) nms <- character(length(val))
            c(tag,
              paste(unlist(map(arg2char, nms, val)), collapse = " "))
        }
        else if(is.logical(val)) {
            if(val) tag
        }
        else
            c(tag, as.character(val))
        out
    }

    unlist(map(arg2char, names(x), x))
}

## Handlers.

make_Weka_control_handler <-
function(options, fun, ...)
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
            if(any(ind)) x[ind] <- lapply(x[ind], fun, ...)
            ## Do *not* coerce to character so that we can compose
            ## several control handlers.
        }
        else {
            ## Old-style character stuff:
            ##   c("-W", "J48")
            x <- as.character(x)        # Just making sure ...
            ind <- which(x %in% options)
            if(any(ind)) x[ind + 1] <- sapply(x[ind + 1], fun, ...)
        }
        x
    }
}

.control_handlers <-
function(...)
{
    ## A little utility to enhance code legibility.
    x <- list(...)
    list(control = mapply(make_Weka_control_handler, names(x), x))
}
         
