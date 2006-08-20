make_Weka_associator <-
function(method, class = NULL)
{
    
    ## Return a function interfacing a Weka associator with constructor
    ## 'method'.

    ## Add to registry.
    classes <- c(class, "Weka_associator")
    meta <- list(method = method, class = classes)
    Weka_interfaces[[sub(".*/", "", method)]] <- meta
        
    out <- function(x, control = NULL) {
        structure(RWeka_build_associator(x, control, method),
                  class = classes)
    }
    class(out) <- c("R_Weka_associator_interface", "R_Weka_interface")
    attr(out, "meta") <- meta
    out
}

RWeka_build_associator <-
function(x, control, method)
{
    instances <- read_data_into_Weka(x)

    ## Build the associator.
    associator <- .jnew(method)
    control <- as.character(control)
    ## <FIXME>
    ## Should we warn if a control argument was given and the associator
    ## does not provide an OptionHandler interface?
    if(length(control) && .has_method(associator, "setOptions"))
        .jcall(associator, , "setOptions", .jarray(control))
    ## </FIXME>
    .jcall(associator, "V", "buildAssociations", instances)

    list(associator = associator)
}

print.Weka_associator <- function(x, ...) {
    writeLines(.jcall(x$associator, "S", "toString"))
    invisible(x)
}


## And now for the really cool stuff:

Apriori <-
    make_Weka_associator("weka/associations/Apriori", "Apriori")
Tertius <-
    make_Weka_associator("weka/associations/Tertius", "Tertius")
