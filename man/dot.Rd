\name{dot}
\alias{write_to_dot}
\alias{write_to_dot.Weka_classifier}
\title{Create DOT Representations}
\description{
  Write a DOT language representation of an object for processing via
  GraphViz.
}
\usage{
write_to_dot(x, con = stdout(), \dots)
\method{write_to_dot}{Weka_classifier}(x, con = stdout(), \dots)
}
\arguments{
  \item{x}{an \R object.}
  \item{con}{a \link{connection} for writing the representation to.}
  \item{\dots}{additional arguments to be passed from or to methods.}
}
\details{
  Graphviz (\url{http://www.graphviz.org}) is open source graph
  visualization software providing several main graph layout programs,
  of which \code{dot} makes \dQuote{hierarchical} or layered drawings of
  directed graphs, and hence is typically most suitable for visualizing
  classification trees.

  Using \code{dot}, the representation in file \file{foo.dot} can be
  transformed to PostScript or other displayable graphical formats using
  (a variant of) \code{dot -Tps foo.dot >foo.ps}.

  Some Weka classifiers (e.g., tree learners such as J48 and M5P)
  implement a \dQuote{Drawable} interface providing DOT representations
  of the fitted models.  For such classifiers, the \code{write_to_dot}
  method writes the representation to the specified connection. 
}
\keyword{graphs}
