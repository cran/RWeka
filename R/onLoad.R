.onLoad <-
function(libname, pkgname) {
    .jinit(system.file("jar",
                       c("weka.jar", "RWeka.jar"),
                       package = pkgname,
                       lib.loc = libname))
}
