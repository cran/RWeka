\name{Weka_tokenizers}
\alias{AlphabeticTokenizer}
\alias{NGramTokenizer}
\alias{WordTokenizer}
\title{R/Weka Tokenizers}
\description{
  R interfaces to Weka tokenizers.
}
\usage{
AlphabeticTokenizer(x, control = NULL)
NGramTokenizer(x, control = NULL)
WordTokenizer(x, control = NULL)
}
\arguments{
  \item{x}{a character vector with strings to be tokenized.}
  \item{control}{an object of class \code{\link{Weka_control}}, or a
    character vector of control options, or \code{NULL} (default).
    Available options can be obtained on-line using the Weka Option
    Wizard \code{\link{WOW}}, or the Weka documentation.}
}
\details{
  \code{AlphabeticTokenizer} is an alphabetic string tokenizer, where
  tokens are to be formed only from contiguous alphabetic sequences.

  \code{NGramTokenizer} splits strings into \eqn{n}-grams with given
  minimal and maximal numbers of grams.

  \code{WordTokenizers} is a simple word tokenizer.
}
\value{
  A character vector with the tokenized strings.
}
\keyword{character}
