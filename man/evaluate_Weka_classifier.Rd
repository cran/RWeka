\name{evaluate_Weka_classifier}
\alias{evaluate_Weka_classifier}
\title{Model Statistics for R/Weka Classifiers}
\description{
  Compute model performance statistics for a fitted Weka classifier.
}
\usage{
evaluate_Weka_classifier(object, newdata = NULL, cost = NULL, 
                         numFolds = 0, complexity = FALSE,
                         class = FALSE, seed = NULL, ...)
}
\arguments{
  \item{object}{a \code{Weka_classifier} object.}
  \item{newdata}{an optional data frame in which to look for variables
    with which to evaluate.  If omitted or \code{NULL}, the training
    instances are used.}
  \item{cost}{a square matrix of (mis)classification costs.}
  \item{numFolds}{the number of folds to use in cross-validation.}
  \item{complexity}{option to include entropy-based statistics.}
  \item{class}{option to include class statistics.}
  \item{seed}{optional seed for cross-validation.}
  \item{\dots}{further arguments passed to other methods (see details).}
}
\details{
  The function computes and extracts a non-redundant set of performance 
  statistics that is suitable for model interpretation. By default the 
  statistics are computed on the training data.

  Currently argument \code{\dots} only supports the logical variable
  \code{normalize} which tells Weka to normalize the cost matrix so that
  the cost of a correct classification is zero.
}
\value{
  An object of class \code{Weka_classifier_evaluation}, a list of the
  following components:

  \item{string}{character, concatenation of the string representations
    of the performance statistics.}
  \item{details}{vector, base statistics, e.g., the percentage of
    instances correctly classified, etc.}
  \item{detailsComplexity}{vector, entropy-based statistics (if
    selected).}
  \item{detailsClass}{matrix, class statistics, e.g., the true positive
    rate, etc., for each level of the response variable (if selected).}
  \item{confusionMatrix}{table, cross-classification of true and
    predicted classes.}
}
\references{
  I. H. Witten and E. Frank (2005).
  \emph{Data Mining: Practical Machine Learning Tools and Techniques}.
  2nd Edition, Morgan Kaufmann, San Francisco.    
}
\examples{
## Use some example data.
w <- read.arff(system.file("arff","weather.nominal.arff", 
	       package = "RWeka"))

## Identify a decision tree.
m <- J48(play~., data = w)
m

## Use 10 fold cross-validation.
e <- evaluate_Weka_classifier(m,
                              cost = matrix(c(0,2,1,0), ncol = 2),
                              numFolds = 10, complexity = TRUE,
                              seed = 123, class = TRUE)
e
summary(e)
e$details
}
\keyword{models}
