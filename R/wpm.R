WPM <-
function(cmd, ...)
{
    ## See
    ## <http://weka.sourceforge.net/doc.dev/weka/core/WekaPackageManager.html>.
        
    cmd <- cmd[1L]
    cmds <- c(## Corresponding to command line options:
              "refresh-cache",
              "package-info",
              "list-packages",
              "install-package", "remove-package",
              ## (command line uses -uninstall-package)
              "toggle-load-status",
              ## R specific:
              "load-packages",
              ## Internal use only:
              ".check-installed-and-load"
              )
    pos <- pmatch(tolower(cmd), cmds)
    if(is.na(pos))
        stop(gettextf("Invalid package manager command '%s'.", cmd),
             domain = NA)
    cmd <- cmds[pos]

    args <- as.character(list(...))

    wpm <- .jnew("weka.core.WekaPackageManager")

    ## * -refresh-cashe
    ## This is
    ##   public static java.lang.Exception
    ##   weka.core.WekaPackageManager.refreshCache(java.io.PrintStream[])
    ## We could try to capture progress into a suitable output stream,
    ## but for now simply allow writing to stdout ... sort of.
    ## Messy ... similar for the others.

    ## Note that as of 8982 the WekaPackageManager class main() method
    ## calls System.exit(0), so using main() is no longer feasible.  We
    ## currently patch the upstream Weka sources for RWekajars, but
    ## should really rewrite the WPM() code to use the appropriate
    ## WekaPackageManager methods directly, instead of main().

    if(cmd == "refresh-cache") {
        .jcall(wpm,
               "Ljava/lang/Exception;",
               "refreshCache",
               .jarray(.jfield("java/lang/System", , "out"),
                       "java/io/PrintStream"))
        return(invisible())
    }

    ## Capture Java output.
    bos <- .jnew("java/io/ByteArrayOutputStream")
    out <- .jfield("java/lang/System", , "out")
    .jcall("java/lang/System", "V", "setOut",
           .jnew("java/io/PrintStream",
                 .jcast(bos,"java/io/OutputStream")))
    err <- .jfield("java/lang/System", , "err")
    .jcall("java/lang/System", "V", "setErr",
           .jnew("java/io/PrintStream",
                 .jcast(bos,"java/io/OutputStream")))

    on.exit({
        ## Stop redirecting Java messages.
        .jcall("java/lang/System", "V", "setOut", out)
        .jcall("java/lang/System", "V", "setErr", err)
        ## And display them.
        message(.jcall(bos, "Ljava/lang/String;", "toString"))
    })

    switch(EXPR = cmd,    
           ".check-installed-and-load" = {
               ## Need to write code ourselves ...
               arg <- args[1L]
               if(is.na(arg))
                   stop(gettextf("No package given."),
                        domain = NA)
               ## Explictly throw an error if the package is not
               ## installed.
               installed <-
                   vapply(.jcall(wpm,
                                 "Ljava/util/List;",
                                 "getInstalledPackages"),
                          function(e) .jcall(e, "S", "getName"), "")
               if(is.na(match(arg, installed))) 
                   stop(gettextf("Required Weka package '%s' is not installed.",
                                 arg),
                        domain = NA)
               ## Alternatively, use
               ##   dir <- .jcall(wpm, "Ljava/io/File;", "getPackageHome")
               ##   dir <- file.path(.jcall(dir, "S", "toString"), arg)
               ##   if(!file.exists(dir)) 
               ##       stop(gettextf("Required Weka package '%s' is not installed.",
               ##                     arg),
               ##            domain = NA)
               .jcall(wpm, "V", "loadPackages", FALSE)
               return(invisible())
           },
           "load-packages" = {
               .jcall(wpm, "V", "loadPackages", FALSE)
               return(invisible())
           },               
           "list-packages" = {
               arg <- c(args, "all")[1L]
               tab <- c("all", "installed", "available")
               pos <- pmatch(arg, tab)
               if(is.na(pos))
                   stop("Invalid package manager command '%s %s'",
                        cmd, arg)
               args <- c("-list-packages", tab[pos])
           },
           "package-info" = {
               ## This is somewhat silly ...
               ## But we really need 2 arguments here.
               args <- args[c(1L, 2L)]
               tab <- c("repository", "installed", "archive")
               pos <- pmatch(args[1L], tab)
               if(is.na(pos) || is.na(args[2L]))
                   stop("Invalid package manager command '%s %s %s'",
                        cmd, arg[1L], arg[2L])
               args <- c("-package-info", c(tab[pos], args[2L]))
           },
           "install-package" = {
               if(!length(args))
                   stop(gettextf("No package given."),
                        domain = NA)
               args <- c("-install-package", args)
           },
           "remove-package" = {
               arg <- args[1L]
               if(is.na(arg))
                   stop(gettextf("No package given."),
                        domain = NA)
               args <- c("-uninstall-package", arg)
           },
           "toggle-load-status" = {
               if(!length(args))
                   stop(gettextf("No package given."),
                        domain = NA)
               args <- c("-toggle-load-status", args)
           }
           )
    
    .jcall(wpm, "V", "main", .jarray(args))
}

make_Weka_package_loader <-
function(p)
    function() {
        WPM(".check-installed-and-load", p)
    }

Weka_package_class_loader_manager <-
local({
    man <- NULL          
    function(new) {
        if(!missing(new)) {
            man <<- new
        }
        else
            man
    }
})
    
Weka_object_for_name <-
function(name, package = NULL)
{
    ## <FIXME>
    ## Alternatively, should now be able to do:
    ##   if(!is.null(package) &&
    ##      !is.null(man <- Weka_package_class_loader_manager()))
    ##        .jnew(name, class.loader = man$getPackageClassLoaded(package))
    ## but this assumes that in all cases where 'name' is from an
    ## external package the corresponding 'package' is given too:
    ## So let's be defensive for the time being.
    ## </FIXME>
    if(!is.null(man <- Weka_package_class_loader_manager()))
        .jcall(man, "Ljava/lang/Object;", "objectForName",
               as_qualified_name(name))
    else
        .jnew(name)
}
