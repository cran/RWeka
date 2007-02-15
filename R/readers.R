read_data_into_Weka <-
function(x)
{
    ## Write the data to a temporary ARFF file.
    arfff <- tempfile()
    on.exit(unlink(arfff))
    write.arff(x, arfff)

    ## Read the temporary ARFF file into Weka.
    reader <- .jnew("java/io/FileReader", arfff)
    instances <- .jnew("weka/core/Instances",
                       .jcast(reader, "java/io/Reader"))

    instances
}

read_model_frame_into_Weka <-
function(mf)
{
    ## For Weka, always have the response *last* in the ARFF file.
    instances <- read_data_into_Weka(mf[c(seq_along(mf)[-1], 1)])
    .jcall(instances, "V", "setClassIndex",
           as.integer(.jcall(instances, "I", "numAttributes") - 1))
    instances
}

read_instances_from_Weka <-
function(x) {
    arfff <- tempfile()
    on.exit(unlink(arfff))
   
    writeLines(.jcall(x, "Ljava/lang/String;", "toString"), arfff)

    read.arff(arfff)
}

## these are not fully tested yet! a more general problem is that
## the implementations below depend more directly on implementation
## details of Weka whereas those above are less likley to be affected 
## by a release change.
##
## fixme: provide an option for use of the fast transfer functions? 

get_instances_from_Weka <-
function(instances) {
    ## fixme: in the future we could speed things up a bit 
    ##        by a repackaging method in RWekaInterfaces.
    
    out <- vector("list", .jcall(instances, "I", "numAttributes"))
    for (k in seq_len(length(out))) {
        ## in Weka missing values are coded as NaN and the cast 
        ## to double should ensure this for all attribute types.
        out[[k]] <- .jcall(instances, "[D", "attributeToDoubleArray",
                           as.integer(k-1))
        is.na(out[[k]]) <- is.nan(out[[k]])
        attribute <- .jcall(instances, "Lweka/core/Attribute;",
                            "attribute", as.integer(k-1))
        names(out)[k] <- .jcall(attribute, "S", "name")
        ## see Constant Field Values in the Weka documentation.
        switch(.jcall(attribute, "I", "type") + 1,
           {   ## 0 numeric (nothing todo) 
           },
            
           {   ## 1 nominal (Weka value code = R level code - 1)
               idx <- seq(.jcall(attribute, "I", "numValues")) - 1
               out[[k]] <- factor(out[[k]], levels = idx)
               levels(out[[k]]) <-
                   sapply(idx, function(k)
                          .jcall(attribute, "S", "value", as.integer(k))) 
           },
            
           {   ## 2 string
               stop("Type 'string' currently not implemented.")
           },
            
           {   ## 3 date
               stop("Type 'date' currently not implemented.")

               out[[k]] <- out[[k]] / 1e+3
               attr(out[[k]], "tzone") <- ""
               class(out[[k]]) <- c("POSIXt", "POSIXct")
           },

           {   ## 4 relational
               stop("Type 'relational' currently not implemented.")
           }
        )
    }
    out <- data.frame(out)
    ## this could be useful. note that Weka codes a missing 
    ## class attribute as -1.
    attr(out, "classIndex") <- .jcall(instances, "I", "classIndex") + 1
    if (attr(out, "classIndex") == 0)
       attr(out, "classIndex") <- NULL
    out
}

## note that the class index, if any, must be set as an attribute.

put_data_frame_into_Weka <-
function(data) {
    ## build attribute information
    attname <- names(data)
    attinfo <- .jnew("weka/core/FastVector", 
                     as.integer(length(data)))
    for (i in seq_len(length(data))) {
        attribute <- 
            if (is.factor(data[[i]])) {
               levels <- .jnew("weka/core/FastVector", 
                               as.integer(nlevels(data[[i]])))
               sapply(levels(data[[i]]), function(x)
                      .jcall(levels, "V", "addElement", 
                             .jcast(.jnew("java/lang/String", x),
                                    "java/lang/Object")))
               ## shift to Weka's internal coding
               data[[i]] <- as.double(data[[i]]) - 1
               .jnew("weka/core/Attribute", attname[i], levels)
            }
            else if (is.character(data[[i]]))
               stop("Type 'string' currently not implemented.")
            else if (inherits(data[[i]], "POSIXt")) {
               stop("Type 'date' currently not implemented.")
                  
               data[[i]] <- data[[i]] * 1e+3
               .jnew("weka/core/Attribute", attname[i],
                     "yyyy-MM-dd HH:mm:ss")
            }
            else
               .jnew("weka/core/Attribute", attname[i])
        .jcall(attinfo, "V", "addElement",
               .jcast(attribute, "java/lang/Object"))
    }
    
    ## build instances
    n <- dim(data)[1]                   # number of instances
    instances <- .jnew("weka/core/Instances",
                       "R-data-frame",
                       attinfo,
                       as.integer(n))   # capacity
    
    ## set class index.
    classIndex <- attr(data, "classIndex")
    if (!is.null(classIndex))
       .jcall(instances, "V", "setClassIndex",
              as.integer(classIndex - 1))

    ## populate.
    data <- unlist(data, use.names = FALSE)
    data[is.na(data)] <- NaN            # Weka missing value.
    .jcall("RWekaInterfaces", "V", "addInstances",
           instances, .jarray(data), as.integer(n))
    
    instances
}

###
