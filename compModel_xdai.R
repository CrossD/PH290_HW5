#' Compare two models
#'
#' Compare two models according to what their `summary()` method prints out
#'
#' More details: This is a very rudimentary implementation. The two models are compared according to the text output of the `summary()` method
#' 
#' - item1
#' - item2
#' @export
CompModels <- function(mod1, mod2, ...) {
  f1 <- tempfile()
  sink(f1)
  print(summary(mod1))
  sink()

  f2 <- tempfile()
  sink(f2)
  print(summary(mod2))
  sink()
    
  print(diffviewer::visual_diff(f1, f2))
}
