\name{Weka_attribute_evaluators}
\alias{InfoGainAttributeEval}
\alias{GainRatioAttributeEval}
\title{R/Weka Attribute Evaluators}
\description{
  R interfaces to Weka attribute evaluators.
}
\usage{
GainRatioAttributeEval(formula, data, subset, na.action, control = NULL)
InfoGainAttributeEval(formula, data, subset, na.action, control = NULL)
}
\arguments{
  \item{formula}{a symbolic description of a model. Note that for
    unsupervised filters the response can be omitted.}
  \item{data}{an optional data frame containing the variables in the
    model.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  See \code{\link{model.frame}} for
    details.}
  \item{control}{an object of class \code{\link{Weka_control}}, or a
    character vector of control options, or \code{NULL} (default).
    Available options can be obtained on-line using the Weka Option
    Wizard \code{\link{WOW}}, or the Weka documentation.}
}
\value{
  A numeric vector with the figures of merit for the attributes
  specified by the right hand side of \code{formula}.
}
\details{
  \code{GainRatioAttributeEval} evaluates the worth of an attribute by
  measuring the gain ratio with respect to the class.

  \code{InfoGainAttributeEval} evaluates the worth of an attribute by
  measuring the information gain with respect to the class.
}
\examples{
InfoGainAttributeEval(Species ~ . , data = iris)
}
\keyword{models}
