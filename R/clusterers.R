make_Weka_clusterer <-
function(name, class = NULL)
{
    
    ## Return a function interfacing the Weka cluster learner class
    ## 'name'.

    ## Add to registry.
    classes <- c(class, "Weka_clusterer")
    kind <- "R_Weka_clusterer_interface"
    name <- as_JNI_name(name)
    meta <- make_R_Weka_interface_metadata(name, kind, classes)
    Weka_interfaces[[Java_class_base_name(name)]] <- meta
    
    out <- function(x, control = NULL) {
        structure(RWeka_build_clusterer(x, control, name),
                  class = classes)
    }
    make_R_Weka_interface(out, meta)
}

RWeka_build_clusterer <-
function(x, control, name)
{
    instances <- read_data_into_Weka(x)

    ## Build the clusterer.
    clusterer <- .jnew(name)
    control <- as.character(control)
    ## <FIXME>
    ## Should we warn if a control argument was given and the clusterer
    ## does not provide an OptionHandler interface?
    if(length(control) && .has_method(clusterer, "setOptions"))
        .jcall(clusterer, "V", "setOptions", .jarray(control))
    ## </FIXME>
    .jcall(clusterer, "V", "buildClusterer", instances)

    ## Get the class ids.
    class_ids <- .class_ids_for_instances(clusterer, instances)
    if(!is.null(nms <- rownames(x)))
        names(class_ids) <- nms

    list(clusterer = clusterer, class_ids = class_ids)
}

print.Weka_clusterer <- function(x, ...) {
    writeLines(.jcall(x$clusterer, "S", "toString"))
    invisible(x)
}

.class_ids_for_instances <-
function(clusterer, instances)
{
    ## Get the class ids for a fitted Weka clusterer.

    ## Note that Weka starts counting at 0.  We could rewrite this here,
    ## but then class ids returned would not be in sync with output from
    ## Weka's toString() methods.
    
    ## In RWekaInterfaces we set the class label to Double.NaN if the 
    ## instance could not be classified or is less than zero.
    
    if(.has_method(clusterer, "clusterInstance")) {
        class <- .jcall("RWekaInterfaces",
                        "[D",
                        "clusterInstances",
                        .jcast(clusterer, "weka/clusterers/Clusterer"),
                        instances)
        is.na(class) <- is.nan(class)
        as.integer(class)
    }
    else {
        ## If there is no clusterInstance() method, the Weka clusterer
        ## must provide a distributionForInstance() method.
        col <- max.col(.class_memberships_for_instances(clusterer,
                                                        instances))
        as.integer(col - 1)
    }
}

.class_memberships_for_instances <-
function(clusterer, instances)
{
    ## Get the class memberships for a fitted Weka clusterer which
    ## provides a distributionForInstance() method.
    out <- .jcall("RWekaInterfaces",
                  "[D",
                  "distributionForInstances",
                  .jcast(clusterer, "weka/clusterers/Clusterer"),
                  instances)
    ## <FIXME>
    ## Is there anything we can do about dimnames here?
    ## At least, set colnames to 0 : clusterer.numberOfClusters() ...
    matrix(out, ncol = .jcall(clusterer, "I", "numberOfClusters"),
           byrow = TRUE)
    ## </FIXME>
}

predict.Weka_clusterer <-
function(object, newdata = NULL,
         type = c("class_ids", "memberships"), ...)
{
    type <- match.arg(type)

    if(is.null(newdata)) {
        if(type == "class_ids")
            return(object$class_ids)
        else
            stop("Need 'newdata' to predict class memberships.")
    }

    clusterer <- object$clusterer
    instances <- read_data_into_Weka(newdata)
    
    if(type == "class_ids")
        .class_ids_for_instances(clusterer, instances)
    else {
        if(!.has_method(clusterer, "distributionForInstance"))
            stop("Clusterer cannot predict class memberships.")
        .class_memberships_for_instances(clusterer, instances)
    }
}

###

fitted.Weka_clusterer <-
function (object, ...) 
{
        predict(object, ...)
}

