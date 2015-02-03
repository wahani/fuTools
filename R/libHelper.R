.get.fp <- function(x) {
  if (x == 0) return("reached tolerance criterion")
  if (x == 1) return("reached max number of iterations")
  if (x == 2) return("exit with error - last valid x is returned")
}