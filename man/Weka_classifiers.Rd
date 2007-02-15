\name{Weka_classifiers}
\alias{Weka_classifiers}
\title{R/Weka Classifiers}
\description{
  R interfaces to Weka classifiers.
}
\details{
  Supervised learners, i.e., algorithms for classification and
  regression, are termed \dQuote{classifiers} by Weka.  (Numeric
  prediction, i.e., regression, is interpreted as prediction of a
  continuous class.)

  R interface functions to Weka classifiers are created by
  \code{\link{make_Weka_classifier}}, and have formals \code{formula},
  \code{data}, \code{subset}, \code{na.action}, and \code{control}
  (default: none), where the first four have the \dQuote{usual} meanings
  for statistical modeling functions in R, and the last again specifies
  the control options to be employed by the Weka learner.

  Objects created by these interfaces always inherit from class
  \code{Weka_classifier}, and have at least suitable \code{print},
  \code{summary} (via \code{\link{evaluate_Weka_classifier}}), and
  \code{\link[predict.Weka_classifier]{predict}} methods.
}
\seealso{
  Available \dQuote{standard} interface functions are documented in
  \link[=Weka_classifier_functions]{Weka\_classifier\_functions}
  (regression and classification function learners),
  \link[=Weka_classifier_lazy]{Weka\_classifier\_lazy} (lazy learners),
  \link[=Weka_classifier_meta]{Weka\_classifier\_meta} (meta learners),
  \link[=Weka_classifier_rules]{Weka\_classifier\_rules} (rule learners),
  and
  \link[=Weka_classifier_trees]{Weka\_classifier\_trees} (regression and
  classification tree learners).
}
\keyword{models}
\keyword{regression}
\keyword{classif}
