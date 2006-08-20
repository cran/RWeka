.onLoad <-
function(libname, pkgname) {
    ## Currently, rJava has no namespace, so we have to do this:
    require("rJava")
    .jinit(c(system.file("jar", "weka.jar", package = "RWeka"),
             system.file("jar", "RWeka.jar", package = "RWeka")))
}
