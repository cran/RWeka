make_Weka_classifier <-
function(method, class = NULL)
{
    
    ## Return a function interfacing a Weka classifier with constructor
    ## 'method'.
    
    ## Eventually, add more arguments, including:
    ## * a formula handler (e.g., are interactions allowed? etc.)
    ## * a control handler (convert between control lists in R style and
    ##   character strings with Weka options
    ## * a data handler (e.g., are numeric or categorical responses
    ##   allowed? etc.)

    ## Add to registry.
    classes <- c(class, "Weka_classifier")
    meta <- list(method = method, class = classes)
    Weka_interfaces[[sub(".*/", "", method)]] <- meta
        
    out <- function(formula, data, subset, na.action, control = NULL) {
        
        ## The "usual" way of creating a model frame from the call.
        mc <- match.call()
        mf <- mc[c(1, match(c("formula", "data", "subset", "na.action"),
                            names(mc), 0))]
        mf[[1]] <- as.name("model.frame")
        mf <- eval(mf, parent.frame())
    
        structure(c(RWeka_build_classifier(mf, control, method),
                    list(call = mc, terms = attr(mf, "terms"),
                         levels = levels(mf[[1]]))),
                  class = classes)
    }
    class(out) <- c("R_Weka_classifier_interface", "R_Weka_interface")
    attr(out, "meta") <- meta
    out
}

RWeka_build_classifier <-
function(mf, control, method)
{
    instances <- read_model_frame_into_Weka(mf)
    .jcall(instances, "V", "setClassIndex",
           as.integer(.jcall(instances, "I", "numAttributes") - 1))

    ## Build the classifier.
    classifier <- .jnew(method)
    if(length(control))
        .jcall(classifier, , "setOptions", .jarray(control))
    .jcall(classifier, "V", "buildClassifier", instances)

    ## And classify the training instances.
    predictions <- .jcall(.jnew("RWekaInterfaces"), "[D",
                          "classifyInstances",
                          .jcast(classifier,
                                 "weka/classifiers/Classifier"),
                          instances)
    if(!is.null(levels <- levels(mf[[1]])))
        predictions <- levels[predictions + 1]
    
    list(classifier = classifier, predictions = predictions)
}

print.Weka_classifier <- function(x, ...) {
    writeLines(.jcall(x$classifier, "S", "toString"))
    invisible(x)
}

predict.Weka_classifier <-
function(object, newdata = NULL, ...)
{
    ## This should work as a general-purpose interface to getting
    ## predictions from Weka.

    if(is.null(newdata))
        return(object$predictions)

    mf <- model.frame(delete.response(terms(object)), newdata,
                      na.action = object$call$na.action)

    ## Seems that Weka always needs to have a "class" with its
    ## instances, and even know a factor by its levels ...
    classes <- if(!is.null(object$levels))
        factor(NA, levels = object$levels)
    else
        NA
    mf <- cbind(CLASS = classes, mf)

    ## Get new instances into Weka.
    instances <- read_model_frame_into_Weka(mf)
    .jcall(instances, "V", "setClassIndex",
           as.integer(.jcall(instances, "I", "numAttributes") - 1))

    ## Get predictions from Weka.
    out <- .jcall(.jnew("RWekaInterfaces"), "[D",
                  "classifyInstances",
                  .jcast(object$classifier,
                         "weka/classifiers/Classifier"),
                  instances)
    
    ## Post-process predictions for factors.
    if(!is.null(object$levels))
        out <- factor(object$levels[out + 1], levels = object$levels)
    
    out
}

## Class probabilities.
## As TH does not like a method = "prob" argument to predict() methods,
## a separate function for the time being ...

cppredict <-
function(object, newdata, ...)
{
    if(!inherits(object, "Weka_classifier"))
        stop("'object' must be a Weka classifier")
    
    if(is.null(object$levels))
        stop("Can only compute class probabilities for classification problems")
    
    ## We currently do not store the class probabilities (let alone the
    ## whole model frame) for the training instances with the built
    ## classifier, hence always need to try something fancy:
    if(missing(newdata) || is.null(newdata)) {
        ## (Same evaluation is used in expand.model.frame() ...
        newdata <- eval(object$call$data, environment(formula(object)))
    }
    ## Is there a better way?
    mf <- model.frame(delete.response(terms(object)), newdata,
                      na.action = object$call$na.action)

    ## Seems that Weka always needs to have a "class" with its
    ## instances, and even know a factor by its levels ...
    classes <- factor(NA, levels = object$levels)
    mf <- cbind(CLASS = classes, mf)

    ## Get new instances into Weka.
    instances <- read_model_frame_into_Weka(mf)
    .jcall(instances, "V", "setClassIndex",
           as.integer(.jcall(instances, "I", "numAttributes") - 1))

    ## Get predictions from Weka.
    out <- .jcall(.jnew("RWekaInterfaces"), "[D",    
                  "distributionForInstances",
                  .jcast(object$classifier,
                         "weka/classifiers/Classifier"),
                  instances)
    
    matrix(out, nr = NROW(mf),
           dimnames = list(rownames(mf), object$levels))
}

## And now for the really cool stuff:

J48 <- make_Weka_classifier("weka/classifiers/trees/J48",
                            c("J48", "Weka_tree"))
M5P <- make_Weka_classifier("weka/classifiers/trees/M5P",
                            c("M5P", "Weka_tree"))
LMT <- make_Weka_classifier("weka/classifiers/trees/LMT",
                            c("LMT", "Weka_tree"))

LinearRegression <-
    make_Weka_classifier("weka/classifiers/functions/LinearRegression",
                         c("LinearRegression", "Weka_functions"))
Logistic <-
    make_Weka_classifier("weka/classifiers/functions/Logistic",
                         c("Logistic", "Weka_functions"))
