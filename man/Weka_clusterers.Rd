\name{Weka_clusterers}
\alias{Cobweb}
\alias{FarthestFirst}
\alias{SimpleKMeans}
\alias{XMeans}
\alias{DBScan}
\title{R/Weka Clusterers}
\description{
  R interfaces to Weka clustering algorithms.
}
\usage{
Cobweb(x, control = NULL)
FarthestFirst(x, control = NULL)
SimpleKMeans(x, control = NULL)
XMeans(x, control = NULL)
DBScan(x, control = NULL)
}
\arguments{
  \item{x}{an R object with the data to be clustered.}
  \item{control}{an object of class \code{\link{Weka_control}}, or a
    character vector of control options, or \code{NULL} (default).
    Available options can be obtained on-line using the Weka Option
    Wizard \code{\link{WOW}}, or the Weka documentation.}
}
\value{
  A list inheriting from class \code{Weka_clusterers} with components
  including
  \item{clusterer}{a reference (of class
    \code{\link[rJava:jobjRef-class]{jobjRef}}) to a Java object
    obtained by applying the Weka \code{buildClusterer} method to the
    training instances using the given control options.}
  \item{class_ids}{a vector of integers indicating the class to which
    each training instance is allocated (the results of calling the Weka
    \code{clusterInstance} method for the built clusterer and each
    instance).}
}
\details{
  There is a \code{\link[=predict.Weka_clusterer]{predict}} method for
  predicting class ids or memberships from the fitted clusterers.
  
  \code{Cobweb} implements the Cobweb \bibcitep{Fisher:1987} and Classit
  \bibcitep{Gennari+Langley+Fisher:1989} clustering algorithms.
  
  \code{FarthestFirst} provides the
  \dQuote{farthest first traversal algorithm} by
  \bibcitet{Hochbaum+Shmoys:1985},
  which works as a fast simple
  approximate clusterer modeled after simple \eqn{k}-means.

  \code{SimpleKMeans} provides clustering with the \eqn{k}-means
  algorithm.
  
  \code{XMeans} provides \eqn{k}-means extended by an
  \dQuote{Improve-Structure part} and automatically determines the
  number of clusters.

  \code{DBScan} provides the \dQuote{density-based clustering algorithm}
  by \bibcitet{Ester+Kriegel+Sander:1996}.
  Note that noise points are assigned to \code{NA}.
}
\references{
  \bibshow{Ester+Kriegel+Sander:1996,
    Fisher:1987,
    Gennari+Langley+Fisher:1989,
    Hochbaum+Shmoys:1985,
    Pelleg+Moore:2000,
    Witten+Frank:2005}
}
\note{
  \code{XMeans} requires Weka package \pkg{XMeans} to be installed.

  \code{DBScan} requires Weka package \pkg{optics_dbScan} to be
  installed.
}
\examples{
cl1 <- SimpleKMeans(iris[, -5], Weka_control(N = 3))
cl1
table(predict(cl1), iris$Species)

\dontrun{
## Requires Weka package 'XMeans' to be installed.
## Use XMeans with a KDTree.
cl2 <- XMeans(iris[, -5],
              c("-L", 3, "-H", 7, "-use-kdtree",
                "-K", "weka.core.neighboursearch.KDTree -P"))
cl2
table(predict(cl2), iris$Species)
}
}
\keyword{cluster}
