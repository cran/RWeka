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
    the data contain \code{NA}s.}
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
  by Cohen (1995).

  \code{M5Rules} generates a decision list for regression problems using
  separate-and-conquer.  In each iteration it builds an model tree using
  M5 and makes the \dQuote{best} leaf into a rule.  See Hall, Holmes and
  Frank (1999) for more information.

  \code{OneR} builds a simple 1-R classifier, see Holte (1993).

  \code{PART} generates PART decision lists using the approach of Frank
  and Witten (1998).

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
  W. W. Cohen (1995).
  Fast effective rule induction.
  In A. Prieditis and S. Russell (eds.),
  \emph{Proceedings of the 12th International Conference on Machine
    Learning}, pages 115--123.
  Morgan Kaufmann.
  ISBN 1-55860-377-8.
  \url{http://citeseer.ist.psu.edu/cohen95fast.html}
  
  E. Frank and I. H. Witten (1998).
  Generating accurate rule sets without global optimization.
  In J. Shavlik (ed.),
  \emph{Machine Learning: Proceedings of the Fifteenth International
    Conference}.
  Morgan Kaufmann Publishers: San Francisco, CA.
  \url{http://www.cs.waikato.ac.nz/~eibe/pubs/ML98-57.ps.gz}

  M. Hall, G. Holmes, and E. Frank (1999).
  Generating rule sets from model trees.
  \emph{Proceedings of the Twelfth Australian Joint Conference on
    Artificial Intelligence}, Sydney, Australia, pages 1--12.
  Springer-Verlag.
  \url{http://citeseer.ist.psu.edu/holmes99generating.html}

  R. C. Holte (1993).
  Very simple classification rules perform well on most commonly used
  datasets.
  \emph{Machine Learning}, \bold{11}, 63--91. 

  I. H. Witten and E. Frank (2005).
  \emph{Data Mining: Practical Machine Learning Tools and Techniques}.
  2nd Edition, Morgan Kaufmann, San Francisco. 
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
