\name{Weka_converters}
\alias{C45Loader}
\alias{XRFFLoader}
\alias{C45Saver}
\alias{XRFFSaver}
\title{R/Weka File Loaders and Savers}
\description{
  R interfaces to Weka file loaders and savers.
}
\usage{
C45Loader(file)
XRFFLoader(file)
C45Saver(x, file, control = NULL)
XRFFSaver(x, file, control = NULL)
}
\arguments{
  \item{file}{a non-empty character string naming a file to read from or
    write to.}
  \item{x}{the data to be written, preferably a matrix or data frame.
    If not, coercion to a data frame is attempted.}
  \item{control}{an object of class \code{\link{Weka_control}}, or a
    character vector of control options, or \code{NULL} (default).
    Available options can be obtained on-line using the Weka Option
    Wizard \code{\link{WOW}}, or the Weka documentation.}
}
\value{
  Invisibly \code{NULL} for the savers.

  A data frame containing the data from the given file for the loaders.
}
\details{
  \code{C45Loader} and \code{C45Saver} use the format employed by the
  C4.5 algorithm/software, where data is stored in two separate
  \file{.names} and \file{.data} files.

  \code{XRFFLoader} and \code{XRFFSaver} handle \acronym{XRFF}
  (eXtensible attribute-Relation File Format, an \acronym{XML}-based
  extension of Weka's native Attribute-Relation File Format) files.
}
\seealso{
  \code{\link{read.arff}},
  \code{\link{write.arff}}.
}
\keyword{file}
