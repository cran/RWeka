\name{predict_Weka_classifier}
\alias{predict.Weka_classifier}
\alias{fitted.Weka_classifier}
\title{Model Predictions for R/Weka Classifiers}
\description{
  Predicted values based on fitted Weka classifier models.
}
\usage{
\method{predict}{Weka_classifier}(object, newdata = NULL,
        type = c("class", "probability"), ...)
}
\arguments{
  \item{object}{an object of class inheriting from
    \code{Weka_classifier}.}
  \item{newdata}{an optional data frame in which to look for variables
    with which to predict.  If omitted or \code{NULL}, the training
    instances are used.}
  \item{type}{character string determining whether classes should be
    predicted (numeric for regression, factor for classification) or
    class probabilities (only available for classification).  May be
    abbreviated.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  Either a vector with classes or a matrix with the posterior class
  probabilities, with rows corresponding to instances and columns to
  classes.
}
\keyword{models}
