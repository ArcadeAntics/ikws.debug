\name{PRINFO}
\alias{PRINFO}
\title{Inspect a Promise}
\description{
  \code{PRINFO()} returns information about a promise object.
}
\usage{
PRINFO(x, pos = -1, envir = as.environment(pos), inherits = TRUE,
       evaluated = FALSE)
}
\arguments{
  \item{x}{an object name, given as a character string or a symbol.}

  \item{pos, envir}{where to look for the object; if omitted search as if the
    name of the object appeared unquoted in an expression.}

  \item{inherits}{should the enclosing frames of the environment be searched?}

  \item{evaluated}{should \code{x} be evaluated?}
}
\value{
  A list with the following components:

  \item{PRCODE}{

    expression to be evaluated, possibly a bytecode.}

  \item{PRENV}{

    environment in which \code{PRCODE} will be evaluated. \code{NULL} if the
    promise has already been evaluated.}

  \item{PREXPR}{

    if \code{PRCODE} is a bytecode, then the deparsed version of \code{PRCODE},
    otherwise identical to \code{PRCODE}.}

  \item{PRSEEN}{

    integer; has the promise been seen before? \code{0} means unseen, \code{1}
    means has been seen and re-evaluating it will throw an error
    \code{"promise already under evaluation: recursive default argument reference or earlier problems?"},
    \code{2} means has been seen but was interrupted and re-evaluating it will
    throw a warning \code{"restarting interrupted promise evaluation"}.}

  If the promise has been evaluated, it will also have the following component:

  \item{PRVALUE}{

    value returned by evaluating \code{PRCODE} in \code{PRENV}.}
}
