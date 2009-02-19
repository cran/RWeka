\name{Weka_classifier_functions}
\alias{Weka_classifier_functions}
\alias{LinearRegression}
\alias{Logistic}
\alias{SMO}
\title{R/Weka Classifier Functions}
\description{
  R interfaces to Weka regression and classification function learners.
}
\usage{
LinearRegression(formula, data, subset, na.action,
                 control = Weka_control(), options = NULL)
Logistic(formula, data, subset, na.action,
         control = Weka_control(), options = NULL)
SMO(formula, data, subset, na.action,
    control = Weka_control(), options = NULL)
}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.}
  \item{data}{an optional data frame containing the variables in the
    model.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.}
  \item{control}{an object of class \code{\link{Weka_control}} giving
    options to be passed to the Weka learner.  Available options can be
    obtained on-line using the Weka Option Wizard \code{\link{WOW}}, or
    the Weka documentation.}
  \item{options}{a named list of further options, or \code{NULL}
    (default).  See \bold{Details}.}
}
\value{
  A list inheriting from classes \code{Weka_functions} and
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
  There are a \code{\link[predict.Weka_classifier]{predict}} method for
  predicting from the fitted models, and a \code{summary} method based
  on \code{\link{evaluate_Weka_classifier}}.

  \code{LinearRegression} builds suitable linear regression models,
  using the Akaike criterion for model selection.

  \code{Logistic} builds multinomial logistic regression models based on
  ridge estimation (le Cessie and van Houwelingen, 1992).

  \code{SMO} implements John C. Platt's sequential minimal optimization
  algorithm for training a support vector classifier using polynomial or
  RBF kernels.  Multi-class problems are solved using pairwise
  classification.  

  The model formulae should only use the \samp{+} and \samp{-} operators
  to indicate the variables to be included or not used, respectively.

  Argument \code{options} allows further customization.  Currently,
  options \code{model} and \code{instances} (or partial matches for
  these) are used: if set to \code{TRUE}, the model frame or the
  corresponding Weka instances, respectively, are included in the fitted
  model object, possibly speeding up subsequent computations on the
  object.  By default, neither is included.
}
\references{
  J. C. Platt (1998).
  Fast training of Support Vector Machines using Sequential Minimal
  Optimization.
  In B. Schoelkopf, C. Burges, and A. Smola (eds.),
  \emph{Advances in Kernel Methods --- Support Vector Learning}.
  MIT Press.

  I. H. Witten and E. Frank (2005).
  \emph{Data Mining: Practical Machine Learning Tools and Techniques}.
  2nd Edition, Morgan Kaufmann, San Francisco. 
}
\seealso{
  \link[=Weka_classifiers]{Weka\_classifiers}  
}
\examples{
## Linear regression:
## Using standard data set 'mtcars'.
LinearRegression(mpg ~ ., data = mtcars)
## Compare to R:
step(lm(mpg ~ ., data = mtcars), trace = 0)

## Using standard data set 'chickwts'.
LinearRegression(weight ~ feed, data = chickwts)
## (Note the interactions!)

## Logistic regression:
## Using standard data set 'infert'.
STATUS <- factor(infert$case, labels = c("control", "case"))
Logistic(STATUS ~ spontaneous + induced, data = infert)
## Compare to R:
glm(STATUS ~ spontaneous + induced, data = infert, family = binomial())

## Sequential minimal optimization algorithm for training a support
## vector classifier, using am RBF kernel with a non-default gamma
## parameter (argument '-G') instead of the default polynomial kernel
## (from a question on r-help):
SMO(Species ~ ., data = iris,
    control = Weka_control(K =
    list("weka.classifiers.functions.supportVector.RBFKernel", G = 2)))
## In fact, by some hidden magic it also "works" to give the "base" name
## of the Weka kernel class:
SMO(Species ~ ., data = iris,
    control = Weka_control(K = list("RBFKernel", G = 2)))
}
\keyword{models}
\keyword{regression}
\keyword{classif}
