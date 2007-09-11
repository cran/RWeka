.onLoad <-
function(libname, pkgname) {
    ## rJava now has a namespace, but we have trouble using it ...
    require("rJava")
    .jinit(system.file("jar",
                       c("weka.jar", "RWeka.jar"),
                       package = pkgname,
                       lib.loc = libname))
}
