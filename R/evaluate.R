### Evaluate Weka fitted model objects.

## <FIXME>
## We need to check if we got a fitted model because if Java throws R
## does not stop.
## </FIXME>

evaluate_Weka_classifier <- 
function(object, newdata = NULL,
         cost = NULL, numFolds = 0, complexity = FALSE, class = FALSE,
         seed = NULL, ...)
{
    ## For now the usual way ...
    if (is.null(newdata))
        mf <- model.frame(object)
    else {
        ## FIXME 
        mf <- model.frame(object, data = newdata)
        levels <- levels(mf[[1]])
        if (any(is.na(match(object$levels, levels))))
            mf[[1]] <- factor(mf[[1]], levels = object$levels)
    }
    instances  <- read_model_frame_into_Weka(mf)
    result <- list()
    ## Cost sensitive evaluation
    if (is.null(cost))
        evaluation <- .jnew("weka/classifiers/Evaluation", instances)
    else {
       if(!is_square_matrix(cost))
           stop("Argument 'cost' must be a square matrix.")
       costMatrix <- read_costMatrix_into_Weka(cost, ...)
       evaluation <- .jnew("weka/classifiers/Evaluation", instances, 
                           costMatrix)
    }
    ## evaluateModel returns fitted class values
    if (numFolds == 0) {
        ## Weka 3.5.8 has added a varargs argument:
        ##   public double[]
        ##   evaluateModel(Classifier classifier, Instances data,
        ##                 java.lang.Object... forPredictionsPrinting)
        .jcall(evaluation, "[D", "evaluateModel",
               .jcast(object$classifier, "weka/classifiers/Classifier"),
               instances, .jarray(list()))
    }
    else {
        ## Cross validation
        random <- .jnew("java/util/Random")
        if (!is.null(seed))
            .jcall(random, "V", "setSeed", .jlong(seed))
        .jcall(evaluation,"V","crossValidateModel",
               .jcast(object$classifier,"weka/classifiers/Classifier"),
               instances, as.integer(numFolds), random, .jarray(list()))
        result$string <- 
            gettextf("=== %d Fold Cross Validation ===\n", numFolds)
    }
    result$string <-
        paste(result$string, 
              .jcall(evaluation, "S", "toSummaryString", complexity),
              sep = "\n" )
    ## Extractor function
    extractValues <- function(x, ...)
        sapply(x, function(x) .jcall(evaluation, "D", x, ...))
    result$details <-
        extractValues(c("pctCorrect", "pctIncorrect", "pctUnclassified",
                        "kappa", "meanAbsoluteError",
                        "rootMeanSquaredError",
                        "relativeAbsoluteError",
                        "rootRelativeSquaredError"))
    if (!is.null(cost))
       result$detailsCost <- extractValues(c("avgCost"))
    ## <FIXME>
    ## For classifiers which only provide hard probabilistic
    ## predictions, i.e., from {0,1}, the class entropy | scheme can be
    ## meaningless because, according to the source code, if a 
    ## probability is zero 1074 = log2(Double.MIN_VALUE) is added to 
    ## the sum instead of setting to -Inf (Weka versions 3.4.7 + 3.5.2).
    ## this should be fixed in the source code because as each classifier 
    ## implements a distributionForInstance method but does not provide a 
    ## "type" method trapping such output seems to to be difficult ...
    ## </FIXME>
    if (complexity) 
        result$detailsComplexity <-
            extractValues(c("KBInformation", "KBRelativeInformation",
                            "SFPriorEntropy", "SFSchemeEntropy")) 
    if (class) {
       result$string <-
           paste(result$string, 
                 .jcall(evaluation, "S", "toClassDetailsString"),
                 sep = "\n")
       result$detailsClass <-
           t(sapply(object$levels,
                    function(l, x) {
                        k <- as.integer(which(object$levels == l) - 1L)
                        extractValues(x, k)
                    },
                    c("falsePositiveRate", "falseNegativeRate",
                      "precision", "recall", "fMeasure",
                      "areaUnderROC")))
    }
    if(!is.null(cost))
        result$string <-
            paste(result$string,
                  gettext("=== Cost Matrix ===\n"),
                  .jcall(costMatrix, "S", "toString"),
                  sep = "\n")
    result$string <-
        paste(result$string,
              .jcall(evaluation, "S", "toMatrixString"),
              sep = "\n")
    result$confusionMatrix <-
        t(sapply(.jcall(evaluation, "[[D", "confusionMatrix"),
                 .jevalArray))
    if (any(dim(result$confusionMatrix) != length(object$levels)))
        stop("cannot set dimnames on degenerate confusionMatrix")
    dimnames(result$confusionMatrix) <-
        list(object$levels, predicted = object$levels)
    class(result) <- "Weka_classifier_evaluation"
    result
}

##

print.Weka_classifier_evaluation <-
function(x, ...)
{
    cat(x$string)
    invisible(x)
}

## Internal functions

read_costMatrix_into_Weka <-
function(x, normalize = FALSE, ...)
{
    ## Add number of rows and columns as header!
    file <- tempfile()
    on.exit(unlink(file))
    write.table(rbind(dim(x),x), file, row.names = FALSE, 
                                       col.names = FALSE)
    ##
    reader <- .jnew("java/io/FileReader", file)
    x <- .jnew("weka/classifiers/CostMatrix",
               .jcast(reader, "java/io/Reader"))
    if (normalize)
       .jcall(x, "V", "normalize")
    x
}

is_square_matrix <-
function(x, ...)
    is.matrix(x) && nrow(x) == ncol(x)
