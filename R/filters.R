### Weka filters
##
## Note that a lot of the subclasses of Filter implement basic data 
## manipulation, such as removing an attribute.  As these may not be of
## direct interest to R users, they have not been registered.
##
## ceeboo 2006

make_Weka_filter <-
function(method, class = NULL)
{
    ## <FIXME>
    ## Do we need handlers?
    ## </FIXME>
    classes <- c(class, "Weka_filter")
    meta <- list(method = method, class = classes)
    Weka_interfaces[[sub(".*/", "", method)]] <- meta

    out <- function(formula, data, subset, na.action, control = NULL) {
        mc <- match.call()
        mf <- mc[c(1, match(c("formula", "data", "subset", "na.action"),
                            names(mc), 0))]
        mf[[1]] <- as.name("model.frame")
        mf <- eval(mf, parent.frame())
        
        RWeka_use_filter(mf, control, method)
    }
    class(out) <- c("R_Weka_filter_interface", "R_Weka_interface")
    attr(out, "meta") <- meta
    out
}

RWeka_use_filter <-
function(mf, control, method)
{
    ## We do not always need a response variable, e.g., for the class 
    ## of unsupervised filters.

    ## <FIXME>
    ## We do not check the Weka model class.  Thus, the formula may not
    ## fit the model class.  In this case Weka throws, but the rJava
    ## calls do not stop :-(
    ## </FIXME>
    
    if (attr(attr(mf, "terms"), "response") == 0)
       instances <- read_data_into_Weka(mf)
    else
       instances <- read_model_frame_into_Weka(mf)

    ## Build filter.
    filter <- .jnew(method)
    control <- as.character(control)
    if (length(control))
       .jcall(filter, "V", "setOptions", .jarray(control))
    ## This is strange ...
    .jcall(filter, "Z", "setInputFormat", instances)
                       
    ## Unlike Classifier Filter provides a class method for a data set,
    ## i.e., for class Instances :-) 

    instances <- .jcall("weka/filters/Filter", "Lweka/core/Instances;",
                        "useFilter",
                        .jcast(instances,"weka/core/Instances"), 
                        .jcast(filter,"weka/filters/Filter"))
   
    read_instances_from_Weka(instances)
}

## Has no toString method.

print.Weka_filter <-
function(x, ...)
{
    writeLines(.jcall(x$filter, "S", "globalInfo"))
    invisible(x)
}

## Some functions.

Normalize <-
    make_Weka_filter("weka/filters/unsupervised/attribute/Normalize", 
                     "Normalize")
Discretize <-
    make_Weka_filter("weka/filters/supervised/attribute/Discretize", 
                     "Discretize")
