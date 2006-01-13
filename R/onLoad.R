.onLoad <-
function(libname, pkgname) {
    .jinit(c(system.file("jar", "weka.jar", package = "RWeka"),
             system.file("jar", "RWeka.jar", package = "RWeka")))
}
