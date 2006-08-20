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
function(x)
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
  
    return(unlist(lapply(1:length(x), arg2char)))
}
