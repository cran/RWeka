\name{Weka_associators}
\alias{Apriori}
\alias{Tertius}
\title{R/Weka Associators}
\description{
  R interfaces to Weka association rule learning algorithms.
}
\usage{
Apriori(x, control = NULL)
Tertius(x, control = NULL)
}
\arguments{
  \item{x}{an R object with the data to be associated.}
  \item{control}{an object of class \code{\link{Weka_control}}, or a
    character vector of control options, or \code{NULL} (default).
    Available options can be obtained on-line using the Weka Option
    Wizard \code{\link{WOW}}, or the Weka documentation.}
}
\value{
  A list inheriting from class \code{Weka_associators} with components
  including
  \item{associator}{a reference (of class
    \code{\link[rJava:jobjRef-class]{jobjRef}}) to a Java object
    obtained by applying the Weka \code{buildAssociations} method to the
    training instances using the given control options.}
}
\details{
  \code{Apriori} implements an Apriori-type algorithm, which iteratively
  reduces the minimum support until it finds the required number of
  rules with the given minimum confidence.

  \code{Tertius} implements a Tertius-type algorithm.

  See the references for more information on these algorithms.
}
\references{
  R. Agrawal and R. Srikant (1994).
  Fast algorithms for mining association rules in large databases.
  \emph{Proceedings of the  International Conference on Very Large
    Databases}, 478--499.
  Santiago, Chile: Morgan Kaufmann, Los Altos, CA.

  P. A. Flach and N. Lachiche (1999).
  Confirmation-guided discovery of first-order rules with Tertius.
  \emph{Machine Learning}, \bold{42}, 61--95.

  I. H. Witten and Eibe Frank (2005).
  \emph{Data Mining: Practical Machine Learning Tools and Techniques}.
  2nd Edition, Morgan Kaufmann, San Francisco. 
}
\examples{
x <- read.arff(system.file("arff", "contact-lenses.arff",
                           package = "RWeka"))
## Apriori with defaults.
Apriori(x)
## Some options: set required number of rules to 20.
% Apriori(x, c("-N", "20"))
Apriori(x, Weka_control(N = 20))

## Tertius with defaults.
Tertius(x)
## Some options: only classification rules (single item in the RHS).
% Tertius(x, "-S")
Tertius(x, Weka_control(S = TRUE))
}
\keyword{models}
