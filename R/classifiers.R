### Weka classifiers.

## Note that all schemes for numeric or nominal prediction (i.e.,
## classifiers) in Weka extend abstract class "Classifier", and *must*
## provide either distributionForInstance() or classifyInstance().
##
## Note also that class Classifier provides methods
##   getOptions()
##   listOptions()
##   setOptions()
## so that we should be able to safely call these methods.

make_Weka_classifier <-
function(method, class = NULL, handlers = list())
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
    
        structure(c(RWeka_build_classifier(mf, control, method, handlers),
                    list(call = mc, terms = attr(mf, "terms"),
                         levels = levels(mf[[1]]))),
                  class = classes)
    }
    class(out) <- c("R_Weka_classifier_interface", "R_Weka_interface")
    attr(out, "meta") <- meta
    out
}

RWeka_build_classifier <-
function(mf, control, method, handlers)
{
    instances <- read_model_frame_into_Weka(mf)

    ## Build the classifier.
    classifier <- .jnew(method)
    if(is.function(control_handler <- handlers$control))
        control <- control_handler(control)
    if(length(control))
        .jcall(classifier, , "setOptions", .jarray(control))
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

.predictions_for_instances <-
function(classifier, instances)
{
    ## Get the predictions for a fitted Weka classifier.
   
    ## Weka uses NaN for missing values as we do in RWekaInterfaces.
    ## So we have to map to NA.
    
    if(.has_method(classifier, "classifyInstance")) {
        class <- .jcall(.jnew("RWekaInterfaces"), "[D",
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

    out <- .jcall(.jnew("RWekaInterfaces"), "[D",
                  "distributionForInstances",
                  .jcast(classifier, "weka/classifiers/Classifier"),
                  instances)
    matrix(out, nr = .jcall(instances, "I", "numInstances"),
           byrow = TRUE)
}

predict.Weka_classifier <-
function(object, newdata = NULL, type = c("class", "probability"), ...)
{
    ## This should work as a general-purpose interface to getting
    ## predictions from Weka.

    type <- match.arg(type)

    if(type == "probability" && is.null(object$levels))
        stop("Can only compute class probabilities for classification problems")

    if(is.null(newdata)) {
        ## currently only the class predictions for the training 
	## data are stored:
        if(type == "class") return(object$predictions)
        ## but not the probabilities. Hence we need to try something fancy:
	else newdata <- eval(object$call$data, environment(formula(object)))
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
        out <- .predictions_for_instances(object$classifier, instances)
        ## Post-process predictions for factors.
        if(!is.null(object$levels))
            out <- factor(object$levels[out + 1], levels = object$levels)    
    },
    
    "probability" = {
        ## Get predictions from Weka.
        out <- .distribution_for_instances(object$classifier, instances)    
        dimnames(out) <- list(rownames(mf), object$levels)    
    })
    
    out
}

fitted.Weka_classifier <-
function (object, ...) 
{
    predict(object, ...)
}


## Handlers.

make_class_name_expander <-
function(options)
{
    ## Return a function which expands class names specified as the
    ## control arguments following those named by options (e.g., '-W'
    ## for meta learners) if the "base names" are found in the R/Weka
    ## interface registry.
    ##
    ## This is useful, as e.g. for meta learners, '-W' requires the full
    ## class name, but R/Weka users do not necessarily (have to) know
    ## this.

    ## Note that currently, 'control' arguments to R/Weka classifier
    ## interface functions are character vectors, so one cannot simply
    ## give an interface function via '-W'.  Eventually, we might add a
    ## list-style control interface, e.g.
    ##    list("-B", C = 1, W = J48)
    ## which does both the obvious transscription
    ##    C = 1    => "-C", "1"
    ## and the lookup
    ##    W = J48  => "-W", as_Java_class_name(J48$meta$method)

    ## It would also be cool if we could look at the '-W' values, and in
    ## case these do not start with "weka.", find the full class names
    ## using reflectance (if possible) ...

    ## This is really not intended to validate the given control
    ## options.
    
    function(x) {
        ind <- which(x %in% options)
        if(any(ind)) {
            x[ind + 1] <-
                sapply(x[ind + 1],
                       function(s) {
                           full_class_name <-
                               Weka_interfaces[[s]]$method
                           if(is.null(full_class_name))
                               s
                           else
                               as_Java_class_name(full_class_name)
                       })
        }
        x
    }
}        

## And now for the really cool stuff:

## Functions.
LinearRegression <-
    make_Weka_classifier("weka/classifiers/functions/LinearRegression",
                         c("LinearRegression", "Weka_functions"))
Logistic <-
    make_Weka_classifier("weka/classifiers/functions/Logistic",
                         c("Logistic", "Weka_functions"))
SMO <-
    make_Weka_classifier("weka/classifiers/functions/SMO",
                         c("SMO", "Weka_functions"))

## Lazy.
IBk <- make_Weka_classifier("weka/classifiers/rules/IBk",
                            c("IBk", "Weka_lazy"))
LBR <- make_Weka_classifier("weka/classifiers/rules/LBR",
                            c("LBR", "Weka_lazy"))

## Rules.
JRip <- make_Weka_classifier("weka/classifiers/rules/JRip",
                             c("JRip", "Weka_rules"))
M5Rules <- make_Weka_classifier("weka/classifiers/rules/M5Rules",
                                c("M5Rules", "Weka_rules"))
OneR <- make_Weka_classifier("weka/classifiers/rules/OneR",
                             c("OneR", "Weka_rules"))
PART <- make_Weka_classifier("weka/classifiers/rules/PART",
                             c("PART", "Weka_rules"))

## Trees.
J48 <- make_Weka_classifier("weka/classifiers/trees/J48",
                            c("J48", "Weka_tree"))
M5P <- make_Weka_classifier("weka/classifiers/trees/M5P",
                            c("M5P", "Weka_tree"))
LMT <- make_Weka_classifier("weka/classifiers/trees/LMT",
                            c("LMT", "Weka_tree"))
DecisionStump <-
    make_Weka_classifier("weka/classifiers/trees/DecisionStump",
                         c("DecisionStump", "Weka_tree"))

## Meta learners.
.Weka_meta_classifier_handlers <-
    list(control = make_class_name_expander("-W"))
AdaBoostM1 <-
    make_Weka_classifier("weka/classifiers/meta/AdaBoostM1",
                         c("AdaBoostM1", "Weka_meta"),
                         .Weka_meta_classifier_handlers)
Bagging <-
    make_Weka_classifier("weka/classifiers/meta/Bagging",
                         c("Bagging", "Weka_meta"),
                         .Weka_meta_classifier_handlers)
LogitBoost <-
    make_Weka_classifier("weka/classifiers/meta/LogitBoost",
                         c("LogitBoost", "Weka_meta"),
                         .Weka_meta_classifier_handlers)
MultiBoostAB <-
    make_Weka_classifier("weka/classifiers/meta/MultiBoostAB",
                         c("MultiBoostAB", "Weka_meta"),
                         .Weka_meta_classifier_handlers)
Stacking <-
    make_Weka_classifier("weka/classifiers/meta/Stacking",
                         c("Stacking", "Weka_meta"),
                         .Weka_meta_classifier_handlers)
    
