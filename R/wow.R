WOW <-
function(x)
{
    ## Weka Option Wizard.

    name <- get_Java_class(x)
    if(is.null(name))
        stop("Argument 'x' does not specify a known Weka method.")
    
    x <- .jnew(name)

    names <- descriptions <- character()
    lengths <- integer()

    if(.has_method(x, "listOptions")) {
        opt <- .jcall(x, "Ljava/util/Enumeration;", "listOptions")
        while(.jcall(opt, "Z", "hasMoreElements")) {
            o <- .jcall(opt, "Ljava/lang/Object;", "nextElement")
            ## In fact, o is now a weka.core.Option object.
            names <- c(names, .jcall(o, "S", "name"))
            descriptions <-
                c(descriptions, .jcall(o, "S", "description"))
            lengths <- c(lengths, .jcall(o, "I", "numArguments"))
        }
    }

    structure(list(Name = names,
                   Length = lengths,
                   Description = descriptions),
              class = "WOW")
    
}
    
print.WOW <- function(x, ...) {
    if(length(x$Name)) {
        ## Note that we get nothing in case the Weka class has no
        ## listOptions() method.
        out <- mapply(formatDL,
                      sprintf("-%s", x$Name),
                      gsub("\t", " ", x$Description),
                      indent = 8)
        if(any(ind <- (x$Length > 0)))
            out[ind] <- gettextf("%s\n\tNumber of arguments: %d.",
                                 out[ind], x$Length[ind])
        writeLines(out)
    }
    else
        writeLines(gettext("No available information on options."))
    invisible(x)
}
