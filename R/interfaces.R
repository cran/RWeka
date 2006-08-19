list_Weka_interfaces <-
function()
{
    classes <- lapply(mget(objects(Weka_interfaces), Weka_interfaces),
                      "[[", "class")
    o <- split(names(classes),
               mapply("[", classes, sapply(classes, length)))
    tr_db <- list(c("Weka_associator",
                    "Weka_classifier",
                    "Weka_clusterer",
                    "Weka_filter"),
                  c("Associators",
                    "Classifiers",
                    "Clusterers",
                    "Filters"))
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
    if(.has_method(o, "globalInfo"))
        writeLines(c(gettextf("An R interface to Weka class '%s',\nwhich has information",
                              as_Java_class_name(meta$method)),
                     "",
                     strwrap(.jcall(o, "S", "globalInfo"),
                             indent = 2, exdent = 2)))
    else
        writeLines(gettextf("An R interface to Weka class '%s'.",
                            as_Java_class_name(meta$method)))

    writeLines(c("",
                 gettext("Argument list:"),
                 {
                     ax <- deparse(args(x))
                     strwrap(sub("^function", "", ax[-length(ax)]),
                             indent = 2, exdent = 2)
                 },
                 "",
                 gettext("Returns objects inheriting from classes:"),
                 paste(c(" ", meta$class), collapse = " ")))
    invisible(x)
}
