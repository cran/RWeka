list_Weka_interfaces <-
function()
{
    classes <- lapply(mget(objects(Weka_interfaces), Weka_interfaces),
                      "[[", "class")
    o <- split(names(classes),
               mapply("[", classes, sapply(classes, length)))
    tr_db <- list(c("Weka_associator",
                    "Weka_classifier",
                    "Weka_clusterer"),
                  c("Associators",
                    "Classifiers",
                    "Clusterers"))
    names(o) <- tr_db[[2]][match(names(o), tr_db[[1]])]
    o
}

print.R_Weka_interface <-
function(x, ...)
{
    meta <- attr(x, "meta")
    
    ## Seems that not all Weka learners have a globalInfo() method
    ## (e.g., Cobweb for Weka 3.4.6), so be careful.
    o <- .jnew(meta$method)
    if(length(.jmethods(o, "globalInfo")))
        writeLines(c(gettextf("An R interface to Weka class '%s',",
                              gsub("/", ".", meta$method)),
                     "which has information",
                     "",
                     strwrap(.jcall(o, "S", "globalInfo"),
                             indent = 2, exdent = 2)))
    else
        writeLines(gettextf("An R interface to Weka class '%s'.",
                            gsub("/", ".", meta$method)))
    
    writeLines(c("",
                 "Argument list:",
                 {
                     ax <- deparse(args(x))
                     strwrap(sub("^function", "", ax[-length(ax)]),
                             indent = 2, exdent = 2)
                 },
                 "",
                 gettextf("Returns objects inheriting from classes"),
                 paste(c(" ", meta$class), collapse = " ")))
    invisible(x)
}
