\name{predict_Weka_clusterer}
\alias{predict.Weka_clusterer}
\title{Class Predictions for R/Weka Clusterers}
\description{
  Predict class ids or memberships based on fitted Weka clusterers.
}
\usage{
\method{predict}{Weka_clusterer}(object, newdata = NULL,
        type = c("class_ids", "memberships"), ...)
}
\arguments{
  \item{object}{an object of class inheriting from
    \code{Weka_clusterer}.}
  \item{newdata}{an optional data set for predictions are sought. This
    must be given for predicting class memberships. If omitted or
    \code{NULL}, the training instances are used for predicting class
    ids.}
  \item{type}{a character string indicating whether class ids or
    memberships should be returned. May be abbreviated.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  It is only possible to predict class memberships if the Weka clusterer
  provides a \code{distributionForInstance} method.
}
\keyword{cluster}
