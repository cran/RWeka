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

as_JNI_class_name <-
function(x)
    gsub("\\.", "/", x)

as_Java_class_name <-
function(x)
    gsub("/", ".", x)
