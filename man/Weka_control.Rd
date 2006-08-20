\name{Weka_control}
\alias{Weka_control}
\alias{print.Weka_control}
\alias{as.character.Weka_control}
\title{Control Weka Options}
\description{
  Set control options for Weka learners.
}
\usage{
Weka_control(\dots)
}
\arguments{
  \item{\dots}{named arguments of control options, see the details and
    examples.}
}
\details{
  The available options for a Weka learner, \code{foo()} say, can be
  queried by \code{WOW(foo)} and then conveniently set by
  \code{Weka_control()}.  See below for an example.
}

\value{
  A list of class \code{Weka_control} which can be coerced to
  \code{character} for passing it to Weka.
}

\seealso{
  \code{\link{WOW}}
}

\examples{
## Query J4.8 options:
WOW("J48")
## Learn J4.8 tree on iris data with default settings:
J48(Species ~ ., data = iris)
## Learn J4.8 tree with reduced error pruning (-R) and 
## minimum number of instances set to 5 (-M 5):
J48(Species ~ ., data = iris, control = Weka_control(R = TRUE, M = 5))
}
\keyword{documentation}
