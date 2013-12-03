print.fp <- function(x) {
  cat("\nCall:\n", 
      paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Solution:", x$x)
}

summary.fp <- function(x) {
  
  cat("\nSolution: ", x$x, 
      "\nIterations: ", x$iterations, 
      "\nStatus: ", getRetStatus(x$returnStatus), sep = "")
  
  if(!is.null(x$errorMessage)) {
    cat("\nMessage: ", x$errorMessage, 
        "\nCall resulting in error:\n",
        sep = "")
    print(x$errorCall)
  }
  
  invisible(NULL)
}


getRetStatus <- function(x) {
  if (x == 0) return("reached tolerance criterion")
  if (x == 1) return("reached max number of iterations")
  if (x == 2) return("exit with error - last valid x is returned")
}

fp <- function(fun, x0, opts = list(tol = 1e-06,
                                    maxiter = 100)) {
  
  # Helper-Functions
  isTolReached <- function(x, y) {
    all(abs((x - y)/x) < opts$tol)
  }
  
  # Defining the output
  ret <- list(x = -1e08, 
              iterations = 0,
              returnStatus = 0,
              errorMessage = NULL,
              errorCall = NULL,
              call = match.call())
  
  class(ret) <- "fp"
  
  #browser()
  
  while (ret$iterations <= opts$maxiter) {
    
    ret$iterations <- ret$iterations + 1
    tmp <- try(fun(x0))
    
    # Error handling:
    if (inherits(tmp, "try-error")) {
      ret$returnStatus <- 2
      ret$errorMessage <- tmp[1]
      ret$errorCall <- substitute(fun(x0))
      break
    }
      
    ret$x <- tmp
        
    if(isTolReached(ret$x, x0)) 
      break else {
        x0 <- ret$x
      }
  }
  
  if (ret$iterations == opts$maxiter && !isTolReached(ret$x, x0))
    ret$returnStatus <- 1
    
  ret
  
}

x <- fp(function(guess) {
  ret <- mean(c(guess, -1/guess))
  if(ret == -Inf) stop("algorithm ended with -Inf!")
  ret
  }, 1)

tmp$iterations
summary(tmp)


fixedPoint <- function(fun) {
  force(fun)
  function(guess) {
    nextNumber <- fun(guess)
    if (isCloseEnough(guess, nextNumber)) 
      nextNumber else Recall(nextNumber)
  }
}

averageDamp <- function(fun) {
  force(fun)
  function(x) (x + fun(x))/2
}

sqrt <- function(x) fixedPoint(averageDamp(function(y) x/y))(1)
sqrt(2)


#add tracing - print
f <- function (x, a) {
  print(x)
  (x - a) ^ 2
}
optimize(f, c(0, 1), tol = 0.0001, a = 1/3)

#Wrapper - return list-object - elements are function - envirenment contains calls (another object)
#See S3 and S4 objects
wrap.f <- function(f, minimum = TRUE) {
  calls <- NULL
  return(list(f = function(x, ...) {
    calls <<- c(calls, x)
    ifelse(minimum,f(x, ...), -f(x, ...))
  },
              getter = function() return(calls),
              reset = function() calls <<- NULL ))
}

wf <- wrap.f(function (x, a) { (x - a) ^ 2 })
print(optimize(wf$f, c(0, 1), tol = 0.0001, a = 1/3))
wf$getter()
wf$reset()
print(wf$getter())