\name{mkPROMISE}
\alias{mkPROMISE}
\alias{mkEVPROMISE}
\title{Make a Promise}
\description{
  \code{mkPROMISE()} makes a unevaluated promise, \code{mkEVPROMISE()} makes an
  evaluated promise.
}
\usage{
mkPROMISE(expr, envir = parent.frame(1))
mkEVPROMISE(expr, value)
}
\arguments{
  \item{expr}{expression of the promise.}

  \item{envir}{the environment in which \code{expr} is to be evaluated.}

  \item{value}{value of the promise.}
}
\value{
  A promise object, which would usually be directly assigned to a variable.
}
