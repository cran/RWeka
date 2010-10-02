\name{Weka_interfaces}
\title{R/Weka interfaces}
\alias{make_Weka_associator}
\alias{make_Weka_classifier}
\alias{make_Weka_clusterer}
\alias{make_Weka_filter}
\alias{list_Weka_interfaces}
\alias{make_Weka_package_loader}
\description{
  Create an R interface to an existing Weka learner/filter, or show the
  available interfaces.
}
\usage{
make_Weka_associator(name, class = NULL, init = NULL)
make_Weka_classifier(name, class = NULL, handlers = list(), init = NULL)
make_Weka_clusterer(name, class = NULL, init = NULL)
make_Weka_filter(name, class = NULL, init = NULL)
list_Weka_interfaces()
make_Weka_package_loader(p)
}
\arguments{
  \item{name}{a character string giving the fully qualified name of
    a Weka learner/filter class in \acronym{JNI} notation.}
  \item{class}{\code{NULL} (default), or a character vector giving the
    names of R classes the objects returned by the interface function
    should inherit from in addition to the default ones (for
    representing associators, classifiers, and clusterers).}
  \item{handlers}{a named list of special handler functions, see
    \bold{Details}.}
  \item{init}{\code{NULL}, or a function with no arguments to be called
    when the interface is used for building the learner/filter, or
    queried for available options via \code{\link{WOW}}.  Typically,
    this is used for loading Weka packages when interfacing
    functionality in these.}
  \item{p}{a character string naming a Weka package to be loaded via
    \code{\link{WPM}}.}
}
\details{
  \code{make_Weka_associator} and \code{make_Weka_clusterer} create an R
  function providing an interface to a Weka association learner or a
  Weka clusterer, respectively.  This interface function has formals
  \code{x} and \code{control = NULL}, representing the training
  instances and control options to be employed.  Objects created by
  these interface functions always inherit from classes
  \code{Weka_associator} and \code{Weka_clusterer}, respectively,
  and have at least suitable \code{print} methods.  Fitted clusterers
  also have a \code{\link[=predict.Weka_clusterer]{predict}} method.

  \code{make_Weka_classifier} creates an interface function for a Weka
  classifier, with formals \code{formula}, \code{data}, \code{subset},
  \code{na.action}, and \code{control} (default: none), where the first
  four have the \dQuote{usual} meanings for statistical modeling
  functions in R, and the last again specifies the control options to be
  employed by the Weka learner.  Objects created by these interfaces
  always inherit from class \code{Weka_classifier}, and have at least
  suitable \code{print} and
  \code{\link[=predict.Weka_classifier]{predict}} methods.

  \code{make_Weka_filter} creates an interface function for a Weka
  filter, with formals \code{formula}, \code{data}, \code{subset},
  \code{na.action}, and \code{control = NULL}, where the first four have
  the \dQuote{usual} meanings for statistical modeling functions in R,
  and the last again specifies the control options to be employed by the
  Weka filter.  Note that the response variable can be omitted from 
  \code{formula} if the filter is \dQuote{unsupervised}.  Objects
  created by these interface functions are (currently) always of class
  \code{\link{data.frame}}.

  Certain aspects of the interface function can be customized by
  providing handlers.  Currently, only \emph{control} handlers
  (functions given as the \code{control} component of the list of
  handlers) are used for processing the given control arguments before
  passing them to the Weka classifier.  This is used, e.g., by the meta
  learners to allow the specification of registered base learners by
  their \dQuote{base names} (rather their full Weka/Java class names).

  In addition to creating interface functions, the interfaces are
  registered (under the name of the Weka class interfaced), which in
  particular allows the Weka Option Wizard (\code{\link{WOW}}) to
  conveniently give on-line information about available control options
  for the interfaces.

  \code{list_Weka_interfaces} lists the \emph{available} interfaces.

  Finally, \code{make_Weka_package_loader} generates init hooks for
  loading required and already installed Weka packages.

  It is straightforward to register new interfaces in addition to the
  ones package \pkg{RWeka} provides by default.
}
\references{
  K. Hornik, C. Buchta, and A. Zeileis (2009).
  Open-source machine learning: R meets Weka.
  \emph{Computational Statistics}, \bold{24}/2, 225--232,
}
\examples{
## Create an interface to Weka's Naive Bayes classifier.
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
## Note that this has a very useful print method:
NB
## And we can use the Weka Option Wizard for finding out more:
WOW(NB)
## And actually use the interface ...
if(require("e1071", quietly = TRUE) &&
   require("mlbench", quietly = TRUE)) {
    data("HouseVotes84", package = "mlbench")
    model <- NB(Class ~ ., data = HouseVotes84)
    predict(model, HouseVotes84[1:10, -1])
    predict(model, HouseVotes84[1:10, -1], type = "prob")
}
## (Compare this to David Meyer's naiveBayes() in package 'e1071'.)
}
\keyword{interface}
\keyword{models}
