\name{Weka_clusterers}
\alias{Cobweb}
\alias{FarthestFirst}
\alias{SimpleKMeans}
\alias{DBScan}
\title{R/Weka Clusterers}
\description{
  R interfaces to Weka clustering algorithms.
}
\usage{
Cobweb(x, control = NULL)
FarthestFirst(x, control = NULL)
SimpleKMeans(x, control = NULL)
DBScan(x, control = NULL)
}
\arguments{
  \item{x}{an R object with the data to be clustered.}
  \item{control}{a character vector with control options, or \code{NULL}
    (default).  Available options can be obtained on-line using the Weka
    Option Wizard \code{\link{WOW}}, or the Weka documentation.}
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
  There is a \code{\link[predict.Weka_clusterer]{predict}} method for
  class prediction from the fitted clusterers.
  
  \code{Cobweb} implements the Cobweb (Fisher, 1987) and Classit
  (Gennari et al., 1989) clustering algorithms.
  
  \code{FarthestFirst} provides the \dQuote{farthest first traversal
    algorithm} by Hochbaum and Shmoys, which works as a fast simple
  approximate clusterer modelled after simple \eqn{k}-means.

  \code{DBScan} provides the \dQuote{density-based clustering algorithm}
  by Ester, Kriegel, Sander, and Xu. Note that noise points are assigned
  to \code{NA}.
}
\references{
  Ester M., Kriegel H.-P., Sander J., Xu X. (1996).
  A Density-Based Algorithm for Discovering Clusters in Large 
  Spatial Databases with Noise,
  \emph{Proc. 2nd Int. Conf. on Knowledge Discovery and Data Mining (KDD'96)},
  Portland, OR, 226--231.

  D. H. Fisher (1987).
  Knowledge acquisition via incremental conceptual clustering.
  \emph{Machine Learning}, \bold{2}/2, 139--172.

  J. Gennari, P. Langley and D. H. Fisher (1989).
  Models of incremenal concept formation.
  \emph{Artificial Intelligence}, \bold{40}, 11--62.
  
  Hochbaum and Shmoys (1985).
  A best possible heuristic for the \eqn{k}-center problem,
  \emph{Mathematics of Operations Research}, \bold{10}(2), 180--184.

  I. H. Witten and Eibe Frank (2005).
  \emph{Data Mining: Practical Machine Learning Tools and Techniques}.
  2nd Edition, Morgan Kaufmann, San Francisco.
}
\examples{
data(iris)
cl <- SimpleKMeans(iris[, -5], c("-N", "3"))
cl
table(predict(cl), iris$Species)
}
\keyword{cluster}
