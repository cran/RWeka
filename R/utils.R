.has_method <-
function(o, name)
{
    ## <FIXME>
    ## Currently, using rJava reflection segfaults ...
    ## return(TRUE)
    ## </FIXME>

    ## Essentially the same code as in .jmethods().
    
    ## Determine whether Java object o has method 'name'.
    cl <- .jcall(o, "Ljava/lang/Class;", "getClass")
    ms <- .jcall(cl, "[Ljava/lang/reflect/Method;", "getMethods")
    ## This is what currently segfaults ...
    ss <- unlist(lapply(ms, function(x) .jcall(x, "S", "toString")))
    length(grep(paste("\\.", name, "\\(", sep = ''), ss)) > 0
}

.has_Java_method <- 
function(object, name) 
{
    object <- .jcall(object, "Ljava/lang/Class;", "getClass")
    object <- .jcall(object, "[Ljava/lang/reflect/Method;", "getMethods")
    object <- sapply(object, .jcall, "S", "getName")
    match(name, object, nomatch = 0) > 0   
}

make_R_Weka_interface <-
function(f, meta)
    structure(f,
              class = unique(c(meta$kind, "R_Weka_interface")),
              meta = meta)

make_R_Weka_interface_metadata <-
function(name, kind, class = NULL)
    list(name = name, kind = kind, class = class)

as_JNI_name <-
function(x)
    gsub("\\.", "/", x)

as_qualified_name <-
function(x)
    gsub("/", ".", x)

Java_class_base_name <-
function(x)
    sub(".*[/.]", "", x)

get_Java_class <-
function(x)
{
    if(is.character(x)) {
        ## If possibly a full Java class name, return as is.
        ## Otherwise, try treating as the base class name of a Weka
        ## class interfaced and registered.
        if(regexpr("[/.]", x) > -1)
            x
        else
            Weka_interfaces[[x]]$name
    }
    else if(inherits(x, "R_Weka_interface"))
        attr(x, "meta")$name
    else
        NULL
}

get_R_classes_returned <-
function(x)
{
    if(is.character(x))
        Weka_interfaces[[x]]$class
    else if(inherits(x, "R_Weka_interface"))
        attr(x, "meta")$class
    else
        NULL
}

