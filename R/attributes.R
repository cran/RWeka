## Weka attribute evaluation

## An interface mechanism for the Weka attribute evaluation classes
## which implement the weka.AttributeSelection.AttributeEvaluator
## interface), see
## http://weka.sourceforge.net/doc.dev/weka/attributeSelection/AttributeEvaluator.html

make_Weka_attribute_evaluator <-
function(name, class = NULL, init = NULL, package = NULL)
{
    classes <- c(class, "double")
    kind <- "R_Weka_attribute_evaluator_interface"
    name <- as_JNI_name(name)
    meta <- make_R_Weka_interface_metadata(name, kind, classes, init,
                                           package)
    Weka_interfaces[[Java_class_base_name(name)]] <- meta

    out <- function(formula, data, subset, na.action, control = NULL) {
        mc <- match.call()
        mf <- mc[c(1L, match(c("formula", "data", "subset", "na.action"),
                             names(mc), 0L))]
        ## Need 'stats::' for non-standard evaluation:
        mf[[1L]] <- quote(stats::model.frame)
        mf <- eval(mf, parent.frame())

        RWeka_evaluate_attributes(mf, control, name, init, package)
    }

    make_R_Weka_interface(out, meta)
}

RWeka_evaluate_attributes <-
function(mf, control, name, init, package)
{
    if(is.function(init))
        init()
    else if(!is.null(package))
        WPM(".check-installed-and-load", package)

    instances <- read_model_frame_into_Weka(mf)

    evaluator <- Weka_object_for_name(name, package)
    ## Currently we can only handle attribute evaluators which implement
    ## the AttributeEvaluator (but not the SubsetEvaluator) interface.
    if(!.has_method(evaluator, "evaluateAttribute"))
        stop("Can only handle attribute (but not subset) evaluators.")
    
    control <- as.character(control)
    if(length(control))
       .jcall(evaluator, "V", "setOptions", .jarray(control))

    .jcall(evaluator, "V", "buildEvaluator", instances)

    pos <- seq_len(ncol(mf)) - 1L
    pos <- pos[pos != .jcall(instances, "I", "classIndex")]
    out <- vapply(pos,
                  function(p)
                      .jcall(evaluator, "D", "evaluateAttribute", p),
                  0)
    names(out) <- colnames(mf)[pos + 1L]

    out
}
