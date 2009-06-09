.onLoad <-
function(libname, pkgname) {
    ## <FIXME>
    ## Add
    ##   lib.loc = libname
    ## when rJava adds the lib.loc argument.
    .jpackage(pkgname)
    ## </FIXME>
}
