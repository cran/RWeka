\name{Weka_classifier_lazy}
\alias{Weka_classifier_lazy}
\alias{IBk}
\alias{LBR}
\title{R/Weka Lazy Learners}
\description{
  R interfaces to Weka lazy learners.
}
\usage{
IBk(formula, data, subset, na.action,
    control = Weka_control(), options = NULL)
LBR(formula, data, subset, na.action,
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
  A list inheriting from classes \code{Weka_lazy} and
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
  There are a \code{\link[=predict.Weka_classifier]{predict}} method for
  predicting from the fitted models, and a \code{summary} method based
  on \code{\link{evaluate_Weka_classifier}}.

  \code{IBk} provides a \eqn{k}-nearest neighbors classifier, see Aha \&
  Kibler (1991).

  \code{LBR} (\dQuote{Lazy Bayesian Rules}) implements a lazy learning
  approach to lessening the attribute-independence assumption of naive
  Bayes as suggested by Zheng \& Webb (2000).

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
  D. Aha and D. Kibler (1991).
  Instance-based learning algorithms.
  \emph{Machine Learning}, \bold{6}, 37--66.
  
  Z. Zheng and G. Webb (2000).
  Lazy learning of Bayesian rules.
  \emph{Machine Learning}, \bold{41}/1, 53--84.
}
\seealso{
  \link[=Weka_classifiers]{Weka\_classifiers}  
}
\keyword{models}
\keyword{regression}
\keyword{classif}
