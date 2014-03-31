\name{Weka_stemmers}
\alias{IteratedLovinsStemmer}
\alias{LovinsStemmer}
\title{R/Weka Stemmers}
\description{
  R interfaces to Weka stemmers.
}
\usage{
IteratedLovinsStemmer(x, control = NULL)
LovinsStemmer(x, control = NULL)
}
\arguments{
  \item{x}{a character vector with words to be stemmed.}
  \item{control}{an object of class \code{\link{Weka_control}}, or a
    character vector of control options, or \code{NULL} (default).
    Available options can be obtained on-line using the Weka Option
    Wizard \code{\link{WOW}}, or the Weka documentation.}
}
\value{
  A character vector with the stemmed words.
}
\references{
  J. B. Lovins (1968),
  Development of a stemming algorithm.
  \emph{Mechanical Translation and Computational Linguistics} \bold{11},
  22--31.
}
\keyword{character}
