\name{fp}
\alias{fp}
\title{Implementation of fixed-point algorithm}
\usage{
fp(fun, x0, opts = list(tol = 1e-06, maxiter = 100), ...)
}
\arguments{
  \item{fun}{the function returning the 'fixed-point' of x}

  \item{x0}{starting value for x. Will be coerced to
  numeric.}

  \item{opts}{list of options used in the algorithm. At
  this point only \code{tol} and \code{maxiter} are
  supported}

  \item{...}{additional arguments passed to \code{fun}}
}
\value{
Returns a list including: \itemize{ \item x numeric. The
solution for x or the value at the last iteration if the
maximum number of iterations are reached or \code{fun}
results in an error.  \item iterations numeric. The
iteration where the algorithm stopped.  \item returnStatus
numeric. Code indicating the return status: \itemize{ \item
0 reached tolerance criterion \item 1 reached max number of
iterations \item 2 exit with error - last valid x is
returned } \item errorMessage if \code{fun} resulted in an
error, the error message \item errorCall if \code{fun}
resulted in an error, the call }
}
\description{
\code{fp} will iteratively evaluate a fixed-point function.
It only supplies the user with some generally usefull and
standardised output.
}
\examples{
iterator <- function(x) {
force(x)
function(guess) {
 ret <- mean(c(guess, x/guess))
 if(ret == -Inf) stop("algorithm ended with -Inf!")
 ret
}
}

# Finding the square root of 2:
x2 <- fp(fun = iterator(2), x0 = 1)

print(x2)
summary(x2)

# Finding the square root of -1 - fun will result in an error:
x_1 <- fp(fun = iterator(-1), x0 = 1)

print(x_1)
summary(x_1)
}

