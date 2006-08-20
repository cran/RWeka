\name{WOW}
\alias{WOW}
\title{Weka Option Wizard}
\description{
  Give on-line information about available control options for Weka
  learners or filters and their R interfaces.
}
\usage{
WOW(x)
}
\arguments{
  \item{x}{a character string giving either the fully qualified name of
    a Weka learner or filter class in JNI notation, or the name of an
    available R interface, or an object obtained from applying these
    interfaces to build an associator, classifier, clusterer, or
    filter.}
}
\details{
  See \code{\link{list_Weka_interfaces}} for the available interface
  functions.
}
\examples{
## The name of an "existing" (registered) interface.
WOW("J48")
## The name of some Weka class (not necessarily in the interface
## registry):
WOW("weka/classifiers/bayes/NaiveBayes")
}
\keyword{documentation}
