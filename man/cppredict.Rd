\name{cppredict}
\alias{cppredict}
\title{Posterior Class Probabilities}
\description{
  Compute posterior class distributions for fitted Weka classifier
  models (if possible) by applying the Weka method
  \code{distributionForInstance} to each test instance.
}
\usage{
cppredict(object, newdata, ...)
}
\arguments{
  \item{object}{an object of class inheriting from
    \code{Weka_classifier}.}
  \item{newdata}{an optional data frame in which to look for variables
    with which to predict.  If omitted or \code{NULL}, the training
    instances are used.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  A matrix with the posterior class probabilities, with rows
  corresponding to instances and columns to classes.
}
\examples{
m <- J48(Species ~ ., data = iris)
cppredict(m)[1:10, ]
}
\keyword{models}
