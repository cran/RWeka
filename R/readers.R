read.arff <-
function(file)
{
    ## Copy the data from a connection to a temporary file.
    if (!is.character(file)) {
        if (!inherits(file, "connection"))
            stop("'file' must be a character string or connection")
        if (!isOpen(file, "r")) {
            open(file, "r")
            on.exit(close(file))
        }
        rf <- file
        file <- tempfile()
        on.exit(unlink(file))
        wf <- file(file, "w")
        writeLines(readLines(rf), wf)
        close(wf)
    }

    ## Read the ARFF file into Weka.
    reader <- .jnew("java/io/FileReader", file)
    instances <- .jnew("weka/core/Instances",
                       .jcast(reader, "java/io/Reader"))

    read_instances_from_Weka(instances)
}


read_model_frame_into_Weka <-
function(mf)
{
    ## Model frame has the class variable in first position.
    read_data_into_Weka(mf, 1L)
}

write.arff <-
function(x, file, eol = "\n")
{
    ## NOTE we could also write from Weka. However, in
    ##      case of a connection we would have to write
    ##      to a temporary file and then copy back.
    if (file == "")
        file <- stdout()
    if(is.character(file)) {
        file <- file(file, 'w')
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("Argument 'file' must be a character string or connection.")
    if (!isOpen(file, "w")) {
        open(file, "w")
        on.exit(close(file))
    }

    if (!is.data.frame(x) && !is.matrix(x))
        x <- data.frame(x)

    instances <- read_data_into_Weka(x)
    text <- .jcall(instances, "Ljava/lang/String;", "toString")

    writeLines(text, file, sep = eol)
}

read_instances_from_Weka <-
function(x)
{
    ## See Weka 3-5-7 classes Instances and Attribute.

    ## Get attribute information
    out <- vector("list", .jcall(x, "I", "numAttributes"))
    for (i in seq_len(length(out))) {
        ## In Weka missing values are coded as NaN and the cast 
        ## to double should ensure this for all attribute types.
        out[[i]] <- .jcall(x, "[D", "attributeToDoubleArray",
                           as.integer(i - 1L))
        attribute <- .jcall(x, "Lweka/core/Attribute;",
                            "attribute", as.integer(i - 1L))
        names(out)[i] <- .jcall(attribute, "S", "name")
        ## See the Constant Field Values in the Weka documentation.
        switch(.jcall(attribute, "I", "type") + 1L,
           {   ## 0 numeric (nothing todo)

               is.na(out[[i]]) <- is.nan(out[[i]])
           },
            
           {   ## 1 nominal (Weka value code = R level code - 1)

               idx <- seq(.jcall(attribute, "I", "numValues")) - 1L
               is.na(out[[i]]) <- is.nan(out[[i]])
               out[[i]] <- factor(out[[i]], levels = idx)
               levels(out[[i]]) <-
                   sapply(idx, function(k)
                          .jcall(attribute, "S", "value", as.integer(k)))
               ## Assume logical (see below).
               if (all(match(levels(out[[i]]), c("FALSE", "TRUE"),
                       nomatch = 0L)))
                   out[[i]] <- out[[i]] == "TRUE"
           },
            
           {   ## 2 string (same as 1 but return as character)

               idx <- seq(.jcall(attribute, "I", "numValues")) - 1L
               is.na(out[[i]]) <- is.nan(out[[i]])
               out[[i]] <- factor(out[[i]], levels = idx)
               levels(out[[i]]) <-
                   sapply(idx, function(k)
                          .jcall(attribute, "S", "value", as.integer(k)))
               out[[i]] <- as.character(out[[i]])
           },
            
           {   ## 3 date (the direct approach is not reliable)
               ##

               ## Format date.
               out[[i]] <- .jcall("RWekaInterfaces", "[Ljava/lang/String;",
                                  "formatDate", attribute, .jarray(out[[i]]),
                                  NA_character_)
               ## Represent date in local time.
               out[[i]] <- as.POSIXct(out[[i]], tz = "")
           },

           {   ## 4 relational
               stop("Type 'relational' currently not implemented.")
           },

           {   ## unknown
               stop("Type not implemented")
           }
        )
    }
    ## NOTE that Weka codes a missing class attribute as -1.
    classIndex <- .jcall(x, "I", "classIndex") + 1L
    if (classIndex && classIndex != length(out))
        out <- c(out[-classIndex], out[classIndex])

    ## Prevent garbling of attribute names, etc.
    data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

read_data_into_Weka <-
function(x, classIndex = 0) {
    ## See the Weka 3-5-7 source code for this insanity
    ## (e.g. string). Note that the class index, if any,
    ## must be set as an attribute.

    ## Build attribute information
    attname <- names(x)
    attinfo <- .jnew("weka/core/FastVector", 
                     as.integer(length(x)))
    for (i in seq_len(length(x))) {
        ## Make logicals into Weka nominals.
        if (is.logical(x[[i]]))
            x[[i]] <- factor(x[[i]])
        attribute <- 
            if (is.factor(x[[i]])) {
               levels <- .jnew("weka/core/FastVector", 
                               as.integer(nlevels(x[[i]])))
               sapply(levels(x[[i]]), function(k)
                      .jcall(levels, "V", "addElement", 
                             .jcast(.jnew("java/lang/String", k),
                                    "java/lang/Object")))
               ## shift to Weka's internal coding
               x[[i]] <- as.double(x[[i]]) - 1
               .jnew("weka/core/Attribute", attname[i], levels)
            }
            else if (is.character(x[[i]])) {
               att <- .jnew("weka/core/Attribute", attname[i],
                            .jnull("weka/core/FastVector"))
               x[[i]] <- as.factor(x[[i]])
               index <- sapply(levels(x[[i]]), function(k)
                               .jcall(att, "I", "addStringValue", k))
               if (any(index < 0))
                   stop("pushing to Type 'string' failed")
               x[[i]] <- as.double(index[as.integer(x[[i]])])

               att
            }
            else if (inherits(x[[i]], "POSIXt")) {
               att <- .jnew("weka/core/Attribute", attname[i],
                            "yyyy-MM-dd HH:mm:ss")
               ## Normalize to local time.
               x[[i]] <- .jcall("RWekaInterfaces", "[D", "parseDate", att,
                                .jarray(format(x[[i]], tz = "")),
                                NA_character_)
               att
            }
            else if (is.numeric(x[[i]]))
               .jnew("weka/core/Attribute", attname[i])
            else
                stop("Type not implemented")
        .jcall(attinfo, "V", "addElement",
               .jcast(attribute, "java/lang/Object"))
    }
    
    ## Build instances.
    n <- dim(x)[1L]                     # number of instances
    instances <- .jnew("weka/core/Instances",
                       "R_data_frame",  # FIXME
                       attinfo,
                       as.integer(n))   # capacity
    
    ## Set class index.
    if (classIndex)
       .jcall(instances, "V", "setClassIndex",
              as.integer(classIndex - 1L))

    ## Populate.
    x <- unlist(x, use.names = FALSE)
    x[is.na(x)] <- NaN                  # Weka missing value.
    .jcall("RWekaInterfaces", "V", "addInstances",
           instances, .jarray(x), as.integer(n))
    
    instances
}

###
