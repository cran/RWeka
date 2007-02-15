\name{Weka_filters}
\alias{Normalize}
\alias{Discretize}
\title{R/Weka Filters}
\description{
  R interfaces to Weka filters.
}
\usage{
Normalize(formula, data, subset, na.action, control = NULL)
Discretize(formula, data, subset, na.action, control = NULL)
}
\arguments{
  \item{formula}{a symbolic description of a model. Note that for
    unsupervised filters the response can be omitted.}
  \item{data}{an optional data frame containing the variables in the
    model.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.}
  \item{control}{an object of class \code{\link{Weka_control}}, or a
    character vector of control options, or \code{NULL} (default).
    Available options can be obtained on-line using the Weka Option
    Wizard \code{\link{WOW}}, or the Weka documentation.}
}
\value{
  A data frame.
}
\details{
  \code{Normalize} implements an unsupervised filter that normalizes all
  instances of a dataset to have a given norm.  Only numeric values are 
  considered, and the class attribute is ignored.
    
  \code{Discretize} implements a supervised instance filter that
  discretizes a range of numeric attributes in the dataset into nominal
  attributes.  Discretization is by Fayyad & Irani's MDL method (the
  default).

  Note that these methods ignore nominal attributes, i.e., variables of
  class \code{factor}.
}
\references{
  U. M. Fayyad and K. B. Irani (1993).
  Multi-interval discretization of continuousvalued attributes for
  classification learning.
  \emph{Thirteenth International Joint Conference on Articial
    Intelligence}, 1022--1027.
  Morgan Kaufmann.

  I. H. Witten and E. Frank (2005).
  \emph{Data Mining: Practical Machine Learning Tools and Techniques}.
  2nd Edition, Morgan Kaufmann, San Francisco.
}
\examples{
## Using a Weka data set ...
w <- read.arff(system.file("arff","weather.arff",
	       package = "RWeka"))

## Normalize (response irrelevant)
m1 <- Normalize(~., data = w)
m1

## Discretize
m2 <- Discretize(play ~., data = w)
m2
}
\keyword{models}
\keyword{classif}
