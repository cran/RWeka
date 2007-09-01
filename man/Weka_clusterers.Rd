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
  There is a \code{\link[predict.Weka_clusterer]{predict}} method for
  predicting class ids or memberships from the fitted clusterers.
  
  \code{Cobweb} implements the Cobweb (Fisher, 1987) and Classit
  (Gennari et al., 1989) clustering algorithms.
  
  \code{FarthestFirst} provides the \dQuote{farthest first traversal
    algorithm} by Hochbaum and Shmoys, which works as a fast simple
  approximate clusterer modelled after simple \eqn{k}-means.

  \code{SimpleKMeans} provides clustering with the \eqn{k}-means
  algorithm.
  
  \code{XMeans} provides \eqn{k}-means extended by an
  \dQuote{Improve-Structure part} and automatically determines the
  number of clusters.

  \code{DBScan} provides the \dQuote{density-based clustering algorithm}
  by Ester, Kriegel, Sander, and Xu. Note that noise points are assigned
  to \code{NA}.
}
\references{
  M. Ester, H.-P. Kriegel, J. Sander, and X. Xu (1996).
  A Density-Based Algorithm for Discovering Clusters in Large Spatial
  Databases with Noise.
  \emph{Proceedings of the Second International Conference on Knowledge
    Discovery and Data Mining (KDD'96)},
  Portland, OR, 226--231.
  AAAI Press.

  D. H. Fisher (1987).
  Knowledge acquisition via incremental conceptual clustering.
  \emph{Machine Learning}, \bold{2}/2, 139--172.

  J. Gennari, P. Langley and D. H. Fisher (1989).
  Models of incremental concept formation.
  \emph{Artificial Intelligence}, \bold{40}, 11--62.
  
  D. S. Hochbaum and D. B. Shmoys (1985).
  A best possible heuristic for the \eqn{k}-center problem,
  \emph{Mathematics of Operations Research}, \bold{10}(2), 180--184.

  D. Pelleg and A. W. Moore (2006).
  X-means: Extending K-means with Efficient Estimation of the Number of
  Clusters.
  In: \emph{Seventeenth International Conference on Machine Learning},
  727--734.
  Morgan Kaufmann.

  I. H. Witten and E. Frank (2005).
  \emph{Data Mining: Practical Machine Learning Tools and Techniques}.
  2nd Edition, Morgan Kaufmann, San Francisco.
}
\examples{
cl1 <- SimpleKMeans(iris[, -5], Weka_control(N = 3))
cl1
table(predict(cl1), iris$Species)

## Use XMeans with a KDTree.
cl2 <- XMeans(iris[, -5],
              c("-L", 3, "-H", 7, "-use-kdtree",
                "-K", "weka.core.neighboursearch.KDTree -P"))
cl2
table(predict(cl2), iris$Species)
}
\keyword{cluster}
