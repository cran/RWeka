\name{WPM}
\alias{WPM}
\title{Weka Package Manager}
\description{
  Manage Weka packages.
}
\usage{
WPM(cmd, ...)
}
\arguments{
  \item{cmd}{a character string specifying the action to be performed.
    Must be one of \code{"refresh-cache"}, \code{"list-packages"},
    \code{"install-package"}, \code{"remove-package"}, or
    \code{"load-package"} (or a unique abbreviation thereof).}
  \item{...}{character strings giving further arguments required for the
    action to be performed.  See \bold{Details}.}
}
\details{
  Available actions and respective additional arguments are as follows.
  \describe{
    \item{\code{"refresh-cache"}}{Refresh the cached copy of the package
      meta data from the central package repository.}
    \item{\code{"list-packages"}}{print information (version numbers and
      short descriptions) about packages as specified by an additional
      keyword which must be one of \code{"all"} (all packages the system
      knows about), \code{"installed"} (all packages installed locally),
      or (\code{"available"} (all known packages not installed locally),
      or a unique abbreviation thereof.}
    \item{\code{"package-info"}}{print information (metadata) about a
      package.  Requires two additional character string arguments: a
      keyword and the package name.  The keyword must be one of
      \code{"repository"} (print info from the repository) or 
      \code{"installed"} (print info on the installed version), or a
      unique abbreviation thereof.}
    \item{\code{"install-package"}}{install a package as specified by an
      additional character string giving its name.  (In principle, one
      could also provide a file path or URL to a zip file.)}
    \item{\code{"remove-package"}}{remove a given (installed) package.}
    \item{\code{"load-package"}}{load a given package by adding its jars
      to the Java classpath.}
  }
}
\examples{
\dontrun{
## Start by building/refreshing the cache.
WPM("refresh-cache")
## Show the packages installed locally.
WPM("list-packages", "installed")
## Show the packages available from the central Weka package
## repository and not installed locally.
WPM("list-packages", "available")
## Show repository information about package XMeans.
WPM("package-info", "repository", "XMeans")
}
}
