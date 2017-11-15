#' Create matrix of model fit, residuals, and residuals as a proportion of the fitted values.
#'
#' @param model An lm or glm object.
#' @return A matrix.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitres(model.lm)


#####################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE: Generate fitted values and residuals into one matrix.
###
### INPUT: lm/glm object. E.g. model.lm <- lm(y ~ x).
###
### OUTPUT: matrix.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). fitres.r. https://github.com/robertschnitman/schnitr
#####################################################################################

fitres <- function(model) {
  fit          <- predict(model)
  residual     <- resid(model)
  residual_pct <- residual/fit

  cbind(fit, residual, residual_pct)
}
