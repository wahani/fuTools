#' @S3method print fp
print.fp <- function(x, ...) {
  cat("\nCall:\n", 
      paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Solution:", x$x)
  invisible(x)
}

#' @S3method summary fp
summary.fp <- function(object, ...) {
  
  cat("\nSolution:", object$x, 
      "\nIterations:", object$iterations, 
      "\nStatus:", .get.fp(object$returnStatus), sep = " ")
  
  if(!is.null(object$errorMessage)) {
    cat("\nMessage: ", object$errorMessage, 
        "\nCall resulting in error:\n",
        sep = "")
    print(object$errorCall)
  }
  
}
