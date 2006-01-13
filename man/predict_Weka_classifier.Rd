\name{predict_Weka_classifier}
\alias{predict.Weka_classifier}
\title{Model Predictions for R/Weka Classifiers}
\description{
  Predicted values based on fitted Weka classifier models.
}
\usage{
\method{predict}{Weka_classifier}(object, newdata = NULL, ...)
}
\arguments{
  \item{object}{an object of class inheriting from
    \code{Weka_classifier}.}
  \item{newdata}{an optional data frame in which to look for variables
    with which to predict.  If omitted or \code{NULL}, the training
    instances are used.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\keyword{models}
