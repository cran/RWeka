### Weka classifiers.

## Note that all schemes for numeric or nominal prediction (i.e.,
## classifiers) in Weka extend abstract class "Classifier", and *must*
## provide either distributionForInstance() or classifyInstance().
##
## Note also that class Classifier provides methods
##   getOptions()
##   listOptions()
##   setOptions()
## (in fact, Weka's OptionHandler interface) so that we should be able
## to safely call these methods.

make_Weka_classifier <-
function(name, class = NULL, handlers = list())
{
    
    ## Return a function interfacing the Weka classification learner
    ## class 'name'.
    
    ## Eventually, add support for more handlers, including:
    ## * a formula handler (e.g., are interactions allowed? etc.)
    ## * a data handler (e.g., are numeric or categorical responses
    ##   allowed? etc.)

    ## Add to registry.
    classes <- c(class, "Weka_classifier")
    kind <- "R_Weka_classifier_interface"
    name <- as_JNI_name(name)
    meta <- make_R_Weka_interface_metadata(name, kind, classes)
    Weka_interfaces[[Java_class_base_name(name)]] <- meta    
        
    out <- function(formula, data, subset, na.action,
                    control = Weka_control())
    {
        ## The "usual" way of creating a model frame from the call.
        mc <- match.call()
        mf <- mc[c(1, match(c("formula", "data", "subset", "na.action"),
                            names(mc), 0))]
        mf[[1]] <- as.name("model.frame")
        mf <- eval(mf, parent.frame())
	
        structure(c(RWeka_build_classifier(mf, control, name, handlers),
                    list(call = mc, terms = attr(mf, "terms"),
                         levels = levels(mf[[1]]))),
                  class = classes)
    }
    make_R_Weka_interface(out, meta)
}

RWeka_build_classifier <-
function(mf, control, name, handlers)
{
    instances <- read_model_frame_into_Weka(mf)

    ## Build the classifier.
    classifier <- .jnew(name)
    control <- as.character(.compose_and_funcall(handlers$control,
                                                 control))
    if(length(control))
        .jcall(classifier, "V", "setOptions", .jarray(control))
    .jcall(classifier, "V", "buildClassifier", instances)

    ## And classify the training instances.
    predictions <- .predictions_for_instances(classifier, instances)
    if(!is.null(levels <- levels(mf[[1]])))
        predictions <- factor(levels[predictions + 1], levels = levels)
    
    list(classifier = classifier, predictions = predictions)
}

print.Weka_classifier <- function(x, ...) {
    writeLines(.jcall(x$classifier, "S", "toString"))
    invisible(x)
}

summary.Weka_classifier <-
function(object, ...)
{
    evaluate_Weka_classifier(object, ...)
}

.predictions_for_instances <-
function(classifier, instances)
{
    ## Get the predictions for a fitted Weka classifier.
   
    ## Weka uses NaN for missing values as we do in RWekaInterfaces.
    ## So we have to map to NA.
    
    if(.has_method(classifier, "classifyInstance")) {
        class <- .jcall("RWekaInterfaces", "[D",
                        "classifyInstances",
                        .jcast(classifier, "weka/classifiers/Classifier"),
                        instances)
        is.na(class) <- is.nan(class)
        class
    }
    else {
        ## If there is no classifyInstance() method, the Weka classifier
        ## must provide a distributionForInstance() method.
        .distribution_for_instances(classifier, instances)
    }
}

.distribution_for_instances <-
function(classifier, instances)
{
    ## Predict the "memberships" for given instances from a fitted Weka
    ## classifier.  (Note that in principle a numeric classifier could
    ## provide just a distributionForInstance() method which in that
    ## case would return the (numeric) predictions.)

    out <- .jcall("RWekaInterfaces", "[D",
                  "distributionForInstances",
                  .jcast(classifier, "weka/classifiers/Classifier"),
                  instances)
    matrix(out, nrow = .jcall(instances, "I", "numInstances"),
           byrow = TRUE)
}

predict.Weka_classifier <-
function(object, newdata = NULL, type = c("class", "probability"), ...)
{
    ## This should work as a general-purpose interface to getting
    ## predictions from Weka.

    type <- match.arg(type)

    if(type == "probability" && is.null(object$levels))
        stop("Can only compute class probabilities for classification problems.")

    if(is.null(newdata)) {
        ## Currently only the class predictions for the training data
        ## are stored:
        if(type == "class") return(object$predictions)
        ## but not the probabilities.  Hence, we need to try something
        ## fancy:
	else newdata <-
            eval(object$call$data, environment(formula(object)))
    }

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

    switch(type,
           "class" = {
               ## Get predictions from Weka.
               out <- .predictions_for_instances(object$classifier,
                                                 instances)
               ## Post-process predictions for factors.
               if(!is.null(object$levels))
                   out <- factor(object$levels[out + 1],
                                 levels = object$levels)    
           },
           "probability" = {
               ## Get predictions from Weka.
               out <- .distribution_for_instances(object$classifier,
                                                  instances)    
               dimnames(out) <- list(rownames(mf), object$levels)    
           })
    
    out
}

fitted.Weka_classifier <-
function (object, ...) 
{
    predict(object, ...)
}

