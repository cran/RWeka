\name{Weka_classifier_trees}
\alias{J48}
\alias{LMT}
\alias{M5P}
\alias{DecisionStump}
\title{R/Weka Classifier Trees}
\description{
  R interfaces to Weka regression and classification tree learners.
}
\usage{
J48(formula, data, subset, na.action, control = NULL)
LMT(formula, data, subset, na.action, control = NULL)
M5P(formula, data, subset, na.action, control = NULL)
DecisionStump(formula, data, subset, na.action, control = NULL)
}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.}
  \item{data}{an optional data frame containing the variables in the
    model.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.}
  \item{control}{a character vector with control options, or \code{NULL}
    (default).  Available options can be obtained on-line using the Weka
    Option Wizard \code{\link{WOW}}, or the Weka documentation.}
}
\value{
  A list inheriting from classes \code{Weka_trees} and
  \code{Weka_classifiers} with components including
  \item{classifier}{a reference (of class
    \code{\link[rJava:jobjRef-class]{jobjRef}}) to a Java object
    obtained by applying the Weka \code{buildClassifier} method to build
    the specified model using the given control options.}
  \item{predictions}{a numeric vector or factor with the model
    predictions for the training instances (the results of calling the
    Weka \code{classifyInstance} method for the built classifier and
    each instance).}
  \item{call}{the matched call.}
}
\details{
  There is a \code{\link[predict.Weka_classifier]{predict}} method for
  predicting from the fitted models.

  \code{J48} generates unpruned or a pruned C4.5 decision trees
  (Quinlan, 1993).

  \code{LMT} implements \dQuote{Logistic Model Trees} (Landwehr, 2003).

  \code{M5P} (where the \samp{P} stands for \sQuote{prime}) generates M5
  model trees using the M5' algorithm, which was introduced in Wang &
  Witten (1997) and enhances the original M5 algorithm by Quinlan
  (1992).

  \code{DecisionStump} implements decision stumps (trees with a single
  split only), which are frequently used as base learners for meta
  learners such as Boosting.
}

\references{
  N. Landwehr (2003).
  \emph{Logistic model trees}.
  Masters thesis, Institute for Computer Science, University of
  Freiburg, Germany.
  \url{http://www.informatik.uni-freiburg.de/~ml/thesis_landwehr2003.html}
  
  R. Quinlan (1993).
  \emph{C4.5: Programs for Machine Learning}.
  Morgan Kaufmann Publishers, San Mateo, CA.

  R. Quinlan (1992).
  Learning with continuous classes.
  \emph{Proceedings of the Australian Joint Conference on Artificial
    Intelligence}, 343--348.
  World Scientific, Singapore. 

  Y. Wang and I. H. Witten (1997).
  Induction of model trees for predicting continuous classes.
  \emph{Proceedings of the European Conference on Machine
    Learning}.
  University of Economics, Faculty of Informatics and Statistics,
  Prague.
}
\examples{
data(iris)
m1 <- J48(Species ~ ., iris)
m1
table(iris$Species, predict(m1))

## Using some Weka data sets ...

## J48
DF2 <- read.arff(system.file("arff", "contact-lenses.arff",
                             package = "RWeka"))
m2 <- J48(`contact-lenses` ~ ., data = DF2)
m2
table(DF2$`contact-lenses`, predict(m2))

## M5P
DF3 <- read.arff(system.file("arff", "cpu.arff",
                             package = "RWeka"))
m3 <- M5P(class ~ ., data = DF3)
m3

## Logistic Model Tree.
DF4 <- read.arff(system.file("arff", "weather.arff",
                             package = "RWeka"))
m4 <- LMT(play ~ ., data = DF4)
m4
table(DF4$play, predict(m4))
}
\keyword{models}
\keyword{regression}
\keyword{classif}
\keyword{tree}
