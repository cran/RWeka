\name{read.arff}
\alias{read.arff}
\title{Read Data from ARFF Files}
\description{
  \code{read.arff} reads data from Weka ARFF files into a data frame.
}
\usage{
read.arff(file)
}
\arguments{
  \item{file}{a character string with the name of the ARFF file to read
    from, or a \code{\link{connection}} which will be opened if
    necessary, and if so closed at the end of the function call.}
}
\value{
  A data frame containing the data from the ARFF file.
}
\references{
  Attribute-Relation File Format
  \url{http://www.cs.waikato.ac.nz/~ml/weka/arff.html}
}
\seealso{
  \code{\link{write.arff}},
  \code{\link{read.table}}
}
\examples{
read.arff(system.file("arff", "contact-lenses.arff",
                      package = "RWeka"))
}
\keyword{file}
\keyword{connection}
