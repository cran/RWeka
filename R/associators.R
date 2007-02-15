make_Weka_associator <-
function(name, class = NULL)
{
    
    ## Return a function interfacing the Weka association learner class
    ## 'name'.

    ## Add to registry.
    classes <- c(class, "Weka_associator")
    kind <- "R_Weka_associator_interface"
    name <- as_JNI_name(name)
    meta <- make_R_Weka_interface_metadata(name, kind, classes)
    Weka_interfaces[[Java_class_base_name(name)]] <- meta
        
    out <- function(x, control = NULL) {
        structure(RWeka_build_associator(x, control, name),
                  class = classes)
    }
    make_R_Weka_interface(out, meta)
}

RWeka_build_associator <-
function(x, control, name)
{
    instances <- read_data_into_Weka(x)

    ## Build the associator.
    associator <- .jnew(name)
    control <- as.character(control)
    ## <FIXME>
    ## Should we warn if a control argument was given and the associator
    ## does not provide an OptionHandler interface?
    if(length(control) && .has_method(associator, "setOptions"))
        .jcall(associator, "V", "setOptions", .jarray(control))
    ## </FIXME>
    .jcall(associator, "V", "buildAssociations", instances)

    list(associator = associator, instances = instances)
}

print.Weka_associator <- function(x, ...) {
    writeLines(.jcall(x$associator, "S", "toString"))
    invisible(x)
}


