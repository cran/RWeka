make_Weka_clusterer <-
function(method, class = NULL)
{
    
    ## Return a function interfacing a Weka clusterer with constructor
    ## 'method'.

    ## Add to registry.
    classes <- c(class, "Weka_clusterer")
    meta <- list(method = method, class = classes)
    Weka_interfaces[[sub(".*/", "", method)]] <- meta
    
    out <- function(x, control = NULL) {
        structure(RWeka_build_clusterer(x, control, method),
                  class = classes)
    }
    class(out) <- c("R_Weka_clusterer_interface", "R_Weka_interface")
    attr(out, "meta") <- meta
    out
}

RWeka_build_clusterer <-
function(x, control, method)
{
    instances <- read_data_into_Weka(x)

    ## Build the clusterer.
    clusterer <- .jnew(method)
    if(length(control))
        .jcall(clusterer, , "setOptions", .jarray(control))
    .jcall(clusterer, "V", "buildClusterer", instances)

    ## Get the class ids.
    class_ids <-
        .jcall(.jnew("RWekaInterfaces"), "[I", "clusterInstances",
               .jcast(clusterer, "weka/clusterers/Clusterer"),
               instances)

    list(clusterer = clusterer, class_ids = class_ids)
}

print.Weka_clusterer <- function(x, ...) {
    writeLines(.jcall(x$clusterer, "S", "toString"))
    invisible(x)
}


## And now for the really cool stuff:

Cobweb <-
    make_Weka_clusterer("weka/clusterers/Cobweb", "Cobweb")
FarthestFirst <-
    make_Weka_clusterer("weka/clusterers/FarthestFirst", "FarthestFirst")
SimpleKMeans <-
    make_Weka_clusterer("weka/clusterers/SimpleKMeans", "SimpleKMeans")
