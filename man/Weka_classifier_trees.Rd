\name{Weka_classifier_trees}
\alias{Weka_classifier_trees}
\alias{J48}
\alias{LMT}
\alias{M5P}
\alias{DecisionStump}
\alias{plot.Weka_tree}
\alias{parse_Weka_digraph}
\title{R/Weka Classifier Trees}
\description{
  R interfaces to Weka regression and classification tree learners.
}
\usage{
J48(formula, data, subset, na.action,
    control = Weka_control(), options = NULL)
LMT(formula, data, subset, na.action,
    control = Weka_control(), options = NULL)
M5P(formula, data, subset, na.action,
    control = Weka_control(), options = NULL)
DecisionStump(formula, data, subset, na.action,
              control = Weka_control(), options = NULL)

parse_Weka_digraph(x, plainleaf = TRUE)
}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.}
  \item{data}{an optional data frame containing the variables in the
    model.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  See \code{\link{model.frame}} for
    details.}
  \item{control}{an object of class \code{\link{Weka_control}} giving
    options to be passed to the Weka learner.  Available options can be
    obtained on-line using the Weka Option Wizard \code{\link{WOW}}, or
    the Weka documentation.}
  \item{options}{a named list of further options, or \code{NULL}
    (default).  See \bold{Details}.}
  \item{x}{a character string with the Weka digraph of a Weka classifier
    tree (see \bold{Details}).}
  \item{plainleaf}{a logical indicating whether the leaves should be
    kept \sQuote{plain}, i.e., with empty labels.}
}
\value{
  A list inheriting from classes \code{Weka_tree} and
  \code{Weka_classifiers} with components including
  \item{classifier}{a reference (of class
    \code{\link[rJava:jobjRef-class]{jobjRef}}) to a Java object
    obtained by applying the Weka \code{buildClassifier} method to build
    the specified model using the given control options.}
  \item{predictions}{a numeric vector or factor with the model
    predictions for the training instances (the results of calling the
    Weka \code{classifyInstance} method for the built classifier and
    each instance).}
  \item{call}{the matched call.}
}
\details{
  There are a \code{\link[=predict.Weka_classifier]{predict}} method for
  predicting from the fitted models, and a \code{summary} method based
  on \code{\link{evaluate_Weka_classifier}}.

  There is also a \code{plot} method for fitted binary \code{Weka_tree}s
  via the facilities provided by package \pkg{partykit}. This converts
  the \code{Weka_tree} to a \code{party} object and then simply calls
  the plot method of this class (see \code{\link[partykit]{plot.party}}).

  Provided the Weka classification tree learner implements the
  \dQuote{Drawable} interface (i.e., provides a \code{graph} method),
  \code{\link{write_to_dot}} can be used to create a DOT representation
  of the tree for visualization via Graphviz or the \pkg{Rgraphviz}
  package.

  \code{J48} generates unpruned or pruned C4.5 decision trees
  \bibcitep{Quinlan:1993}.

  \code{LMT} implements \dQuote{Logistic Model Trees}
  \bibcitep{Landwehr:2003, Landwehr+Hall+Frank:2005}.

  \code{M5P} (where the \samp{P} stands for \sQuote{prime}) generates M5
  model trees using the M5' algorithm, which was introduced in
  \bibcitet{Wang+Witten:1997} and enhances the original M5 algorithm by
  \bibcitet{Quinlan:1992}.

  \code{DecisionStump} implements decision stumps (trees with a single
  split only), which are frequently used as base learners for meta
  learners such as Boosting.

  The model formulae should only use the \samp{+} and \samp{-} operators
  to indicate the variables to be included or not used, respectively.

  Argument \code{options} allows further customization.  Currently,
  options \code{model} and \code{instances} (or partial matches for
  these) are used: if set to \code{TRUE}, the model frame or the
  corresponding Weka instances, respectively, are included in the fitted
  model object, possibly speeding up subsequent computations on the
  object.  By default, neither is included.

  \code{parse_Weka_digraph} can parse the graph associated with a Weka
  tree classifier (and obtained by invoking its \code{graph()} method in
  Weka), returning a simple list with nodes and edges.
}

\references{
  \bibshow{Landwehr:2003,
    Landwehr+Hall+Frank:2005,
    Quinlan:1992,
    Quinlan:1993,
    Wang+Witten:1997,
    Witten+Frank:2005}
}
\seealso{
  \link[=Weka_classifiers]{Weka_classifiers}
}
\examples{
m1 <- J48(Species ~ ., data = iris)

## print and summary
m1
summary(m1) # calls evaluate_Weka_classifier()
table(iris$Species, predict(m1)) # by hand

## visualization
## use partykit package
if(require("partykit", quietly = TRUE)) plot(m1)
## or Graphviz
write_to_dot(m1)
## or Rgraphviz
\dontrun{
library("Rgraphviz")
ff <- tempfile()
write_to_dot(m1, ff)
plot(agread(ff))
}
## The first uses the Weka digraph for the classifier tree
di <- rJava::.jcall(m1$classifier, "S", "graph")
writeLines(di)
## for which we can extract the information about nodes and edges via
parse_Weka_digraph(di)

## Using some Weka data sets ...

## J48
DF2 <- read.arff(system.file("arff", "contact-lenses.arff",
                             package = "RWeka"))
m2 <- J48(`contact-lenses` ~ ., data = DF2)
m2
table(DF2$`contact-lenses`, predict(m2))
if(require("partykit", quietly = TRUE)) plot(m2)

## M5P
DF3 <- read.arff(system.file("arff", "cpu.arff", package = "RWeka"))
m3 <- M5P(class ~ ., data = DF3)
m3
if(require("partykit", quietly = TRUE)) plot(m3)

## Logistic Model Tree.
DF4 <- read.arff(system.file("arff", "weather.arff", package = "RWeka"))
m4 <- LMT(play ~ ., data = DF4)
m4
table(DF4$play, predict(m4))

## Larger scale example.
if(require("mlbench", quietly = TRUE)
   && require("partykit", quietly = TRUE)) {
    ## Predict diabetes status for Pima Indian women
    data("PimaIndiansDiabetes", package = "mlbench")
    ## Fit J48 tree with reduced error pruning
    m5 <- J48(diabetes ~ ., data = PimaIndiansDiabetes,
              control = Weka_control(R = TRUE))
    plot(m5)
    ## (Make sure that the plotting device is big enough for the tree.)
}
}
\keyword{models}
\keyword{regression}
\keyword{classif}
\keyword{tree}
