\name{Weka_classifier_meta}
\alias{Weka_classifier_meta}
\alias{AdaBoostM1}
\alias{Bagging}
\alias{LogitBoost}
\alias{MultiBoostAB}
\alias{Stacking}
\title{R/Weka Meta Learners}
\description{
  R interfaces to Weka meta learners.
}
\usage{
AdaBoostM1(formula, data, subset, na.action,
           control = Weka_control(), options = NULL)
Bagging(formula, data, subset, na.action,
        control = Weka_control(), options = NULL)
LogitBoost(formula, data, subset, na.action,
           control = Weka_control(), options = NULL)
MultiBoostAB(formula, data, subset, na.action,
             control = Weka_control(), options = NULL)
Stacking(formula, data, subset, na.action,
         control = Weka_control(), options = NULL)
}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.}
  \item{data}{an optional data frame containing the variables in the
    model.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.}
  \item{control}{an object of class \code{\link{Weka_control}} giving
    options to be passed to the Weka learner.  Available options can be
    obtained on-line using the Weka Option Wizard \code{\link{WOW}}, or
    the Weka documentation.  Base classifiers with an available R/Weka
    interface (see \code{\link{list_Weka_interfaces}}), can be specified
    (using the \option{W} option) via their \dQuote{base name} as shown
    in the interface registry (see the examples), or their interface
    function.}
  \item{options}{a named list of further options, or \code{NULL}
    (default).  See \bold{Details}.}
}
\value{
  A list inheriting from classes \code{Weka_meta} and
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
  There are a \code{\link[predict.Weka_classifier]{predict}} method for
  predicting from the fitted models, and a \code{summary} method based
  on \code{\link{evaluate_Weka_classifier}}.

  \code{AdaBoostM1} implements the Adaboost M1 method of Freund and
  Schapire (1996).

  \code{Bagging} provides bagging (Breiman, 1996).

  \code{LogitBoost} performs boosting via additive logistic regression
  (Friedman, Hastie and Tibshirani, 2000).

  \code{MultiBoostAB} implements MultiBoosting (Webb, 2000), an
  extension to the AdaBoost technique for forming decision
  committees which can be viewed as a combination of AdaBoost and
  \dQuote{wagging}.

  \code{Stacking} provides stacking (Wolpert, 1992).

  The model formulae should only use the \samp{+} and \samp{-} operators
  to indicate the variables to be included or not used, respectively.

  Argument \code{options} allows further customization.  Currently,
  options \code{model} and \code{instances} (or partial matches for
  these) are used: if set to \code{TRUE}, the model frame or the
  corresponding Weka instances, respectively, are included in the fitted
  model object, possibly speeding up subsequent computations on the
  object.  By default, neither is included.
}
\references{
  L. Breiman (1996).
  Bagging predictors.
  \emph{Machine Learning}, \bold{24}/2, 123--140.

  Y. Freund and R. E. Schapire (1996).
  Experiments with a new boosting algorithm.
  In \emph{Proceedings of the International Conference on Machine
    Learning}, pages 148--156.
  Morgan Kaufmann: San Francisco.

  J. H. Friedman, T. Hastie, and R. Tibshirani (2000).
  Additive logistic regression: A statistical view of boosting.
  \emph{Annals of Statistics}, \bold{28}/2, 337--374.

  G. I. Webb (2000).
  MultiBoosting: A technique for combining boosting and wagging.
  \emph{Machine Learning}, \bold{40}/2, 159--196.

  I. H. Witten and E. Frank (2005).
  \emph{Data Mining: Practical Machine Learning Tools and Techniques}.
  2nd Edition, Morgan Kaufmann, San Francisco. 

  D. H. Wolpert (1992).
  Stacked generalization.
  \emph{Neural Networks}, \bold{5}, 241--259.
}
\seealso{
  \link[=Weka_classifiers]{Weka\_classifiers}  
}
\examples{
## Use AdaBoostM1 with decision stumps.
m1 <- AdaBoostM1(Species ~ ., data = iris,
                 control = Weka_control(W = "DecisionStump"))
table(predict(m1), iris$Species)

summary(m1) # uses evaluate_Weka_classifier()

## Control options for the base classifiers employed by the meta
## learners (apart from Stacking) can be given as follows:
m2 <- AdaBoostM1(Species ~ ., data = iris,
                 control = Weka_control(W = list(J48, M = 30)))
}
\keyword{models}
\keyword{regression}
\keyword{classif}
