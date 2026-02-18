\name{Weka_classifier_rules}
\alias{Weka_classifier_rules}
\alias{JRip}
\alias{M5Rules}
\alias{OneR}
\alias{PART}
\title{R/Weka Rule Learners}
\description{
  R interfaces to Weka rule learners.
}
\usage{
JRip(formula, data, subset, na.action,
     control = Weka_control(), options = NULL)
M5Rules(formula, data, subset, na.action,
        control = Weka_control(), options = NULL)
OneR(formula, data, subset, na.action,
     control = Weka_control(), options = NULL)
PART(formula, data, subset, na.action,
     control = Weka_control(), options = NULL)
}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.}
  \item{data}{an optional data frame containing the variables in the
    model.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  See \code{\link{model.frame}} for
    details.}
  \item{control}{an object of class \code{\link{Weka_control}} giving
    options to be passed to the Weka learner.  Available options can be
    obtained on-line using the Weka Option Wizard \code{\link{WOW}}, or
    the Weka documentation.}
  \item{options}{a named list of further options, or \code{NULL}
    (default).  See \bold{Details}.}
}
\value{
  A list inheriting from classes \code{Weka_rules} and
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

  \code{JRip} implements a propositional rule learner, \dQuote{Repeated
    Incremental Pruning to Produce Error Reduction} (RIPPER), as proposed
  by \bibcitet{Cohen:1995}.

  \code{M5Rules} generates a decision list for regression problems using
  separate-and-conquer.  In each iteration it builds an model tree using
  M5 and makes the \dQuote{best} leaf into a rule.
  See \bibcitet{Holmes+Hall+Frank:1999} for more information.

  \code{OneR} builds a simple 1-R classifier, see \bibcitet{Holte:1993}.

  \code{PART} generates PART decision lists using the approach of
  \bibcitet{Frank+Witten:1998}.

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
  \bibshow{Cohen:1995,
    Frank+Witten:1998,
    Holmes+Hall+Frank:1999,
    Holte:1993,
    Witten+Frank:2005}
}
\seealso{
  \link[=Weka_classifiers]{Weka_classifiers}  
}
\examples{
M5Rules(mpg ~ ., data = mtcars)

m <- PART(Species ~ ., data = iris)
m
summary(m)
}
\keyword{models}
\keyword{regression}
\keyword{classif}
