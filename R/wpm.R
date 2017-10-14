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

    wpm <- Weka_package_manager()

    ## Weka's WekaPackageManager main() needs to call System.exit(0)
    ## when invoked successfully, so cannot be used directly without
    ## patching the Weka sources.  

    ## We have
    ##   public static java.lang.Exception
    ##   weka.core.WekaPackageManager.refreshCache(java.io.PrintStream[])
    ## We could try to capture progress into a suitable output stream,
    ## but for now simply allow writing to stdout ... sort of.

    progress <- .jarray(.jfield("java/lang/System", , "out"),
                        "java/io/PrintStream")
    if(cmd == "refresh-cache") {
        .jcall(wpm,
               "Ljava/lang/Exception;",
               "refreshCache",
               progress)
        return(invisible())
    }

    ## Capture Java output.
    bos <- .jnew("java/io/ByteArrayOutputStream")
    out <- .jfield("java/lang/System", , "out")
    .jcall("java/lang/System", "V", "setOut",
           .jnew("java/io/PrintStream",
                 .jcast(bos, "java/io/OutputStream")))
    err <- .jfield("java/lang/System", , "err")
    .jcall("java/lang/System", "V", "setErr",
           .jnew("java/io/PrintStream",
                 .jcast(bos, "java/io/OutputStream")))

    on.exit({
        ## Stop redirecting Java messages.
        .jcall("java/lang/System", "V", "setOut", out)
        .jcall("java/lang/System", "V", "setErr", err)
        ## And display them.
        msg <- .jcall(bos, "Ljava/lang/String;", "toString")   
        message(msg, appendLF = FALSE)
    })

    ## <FIXME>
    ## Perhaps we could use bos from above for this?
    progress <- .jarray(.jfield("java/lang/System", , "out"),
                        "java/io/PrintStream")
    ## </FIXME>

    if(cmd == "package-info") {
        one <- args[1L]
        two <- args[2L]
        tab <- c("repository", "installed", "archive")
        pos <- pmatch(one, tab)
        if(is.na(pos) || is.na(two))
            stop("Invalid package manager command '%s %s %s'",
                 cmd, one, two)
        one <- tab[pos]
        info <- if(one == "repository") {
                    if(is.na(version <- args[3L]))
                        version <- "Latest"
                    wpm$getRepositoryPackageInfo(two, version)
                } else if(one == "installed") {
                    wpm$getInstalledPackageInfo(two)
                } else                  # Leaves 'archive'
                    wpm$getPackageArchiveInfo(two)
        meta <- info$getPackageMetaData()
        keys <- vapply(meta$keySet(), .jstrVal, "")
        vals <- vapply(keys, function(k) meta$get(k), "")
        names(vals) <- keys
        class(vals) <- "Weka_package_info"
        return(vals)
    }
    else if(cmd == "list-packages") {
        arg <- c(args, "all")[1L]
        tab <- c("all", "installed", "available")
        pos <- pmatch(arg, tab)
        if(is.na(pos))
            stop("Invalid package manager command '%s %s'",
                 cmd, arg)
        arg <- tab[pos]
        info <- if(arg == "all")
                    wpm$getAllPackages()
                else if(arg == "installed")
                    wpm$getInstalledPackages()
                else                    # leaves 'available'
                    wpm$getAvailableCompatiblePackages()

        fun <- function(p) {
            n <- p$getName()
            v_inst <- v_repo <- "----"
            if(p$isInstalled()) {
                p_inst <- wpm$getInstalledPackageInfo(n)
                v_inst <- p_inst$getPackageMetaDataElement("Version")
                p_repo <- wpm$getRepositoryPackageInfo(n)
                if(!is.null(p_repo)) {
                    v_repo <- p_repo$getPackageMetaDataElement("Version")
                    if(is.null(v_repo)) v_repo <- ""
                }
            } else {
                v_repo <- p$getPackageMetaDataElement("Version")
            }
            c(n,
              v_inst,
              v_repo,
              p$getPackageMetaDataElement("Title"))
        }
        
        info <- do.call(rbind, lapply(info, fun))
        if(!length(info))
            info <- matrix(character(), ncol = 4L)
        colnames(info) <-
            c("Installed", "Available", "Package", "Title")
        class(info) <- "Weka_package_listing"
        return(info)
    }
    else if(cmd == "install-package") {
        if(!length(args))
            stop(gettextf("No package given."),
                 domain = NA)
        target <- tolower(args[1L])
        if(startsWith(target, "https://") ||
           startsWith(target, "http://")) {
            .jcall(wpm, "S",
                   "installPackageFromURL",
                   .jnew("java/net/URL", args[1L]),
                   progress)
        } else if(endsWith(target, ".zip")) {
            .jcall(wpm, "S",
                   "installPackageFromArchive",
                   args[1L],
                   progress)
        } else {
            if(is.na(version <- args[2L]))
                version <- "Latest"
            .jcall(wpm, "Z",
                   "installPackageFromRepository",
                   args[1L],
                   version,
                   progress)
        }
        ## <FIXME>
        ## Perhaps use the return value?
        return(invisible())
        ## </FIXME>
    }
    else if(cmd == "remove-package") {
        arg <- args[1L]
        if(is.na(arg))
            stop(gettextf("No package given."),
                 domain = NA)
        .jcall(wpm, "V",
               "uninstallPackage",
               arg,
               FALSE,
               progress)
        ## <FIXME>
        ## Perhaps use the return value?
        return(invisible())
        ## </FIXME>
    }
    else if(cmd == "toggle-load-status") {
        if(!length(args))
            stop(gettextf("No package given."),
                 domain = NA)
        ## toggleLoadStatus() needs ava.util.List<java.lang.String>, not
        ## sure how to best get this ...
        args <- .jnew("java/util/Arrays")$asList(.jarray(args))
        wpm$toggleLoadStatus(args)
        ## <FIXME>
        ## Perhaps use the return value?
        return(invisible())
        ## </FIXME>
    }
    else if(cmd == "load-packages") {
        .jcall(wpm, "V", "loadPackages", FALSE)
        ## <FIXME>
        ## Perhaps use the return value?
        return(invisible())
        ## </FIXME>
    }
    else if(cmd == ".check-installed-and-load") {
        arg <- args[1L]
        if(is.na(arg))
            stop(gettextf("No package given."),
                 domain = NA)
        ## Explictly throw an error if the package is not installed.
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
        ## <FIXME>
        ## Perhaps use the return value?
        return(invisible())
        ## </FIXME>
    }
}

print.Weka_package_info <-
function(x, ...)
{
    write.dcf(rbind(x))
    invisible(x)
}

print.Weka_package_listing <-
function(x, ...)    
{
    writeLines(formatDL(paste(format(c("Installed", x[, 2L])),
                              format(c("Available", x[, 3L]))),
                        c("Package",
                          paste(x[, 1L], x[, 4L], sep = ": "))))
    invisible(x)
}

make_Weka_package_loader <-
function(p)
    function() {
        WPM(".check-installed-and-load", p)
    }

Weka_package_manager <-
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

## Avoid
##   Apr 06, 2017 2:11:37 PM com.github.fommil.netlib.ARPACK <clinit>
#    WARNING: Failed to load implementation from: com.github.fommil.netlib.NativeSystemARPACK
##   Apr 06, 2017 2:11:37 PM com.github.fommil.netlib.ARPACK <clinit>
##   WARNING: Failed to load implementation from: com.github.fommil.netlib.NativeRefARPACK
## messages on startup.
new_Weka_package_manager <-
function() {
    ## Capture Java output.
    bos <- .jnew("java/io/ByteArrayOutputStream")
    out <- .jfield("java/lang/System", , "out")
    .jcall("java/lang/System", "V", "setOut",
           .jnew("java/io/PrintStream",
                 .jcast(bos, "java/io/OutputStream")))
    err <- .jfield("java/lang/System", , "err")
    .jcall("java/lang/System", "V", "setErr",
           .jnew("java/io/PrintStream",
                 .jcast(bos, "java/io/OutputStream")))

    on.exit({
        ## Stop redirecting Java messages.
        .jcall("java/lang/System", "V", "setOut", out)
        .jcall("java/lang/System", "V", "setErr", err)
    })

    .jnew("weka.core.WekaPackageManager")
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
