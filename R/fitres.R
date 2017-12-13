#' Create matrix of model fit, residuals, and residuals margin (residuals as a proportion of the actual values).
#'
#' @param model An lm or glm object.
#' @param data A data frame. If not specified, then the original data is used (i.e. model.frame(model)).
#' @param type String. Prediction type depends on whether the object is lm ('response', 'terms') or glm ('link', 'response', 'terms'). (See ?predict.lm and ?predict.glm for details).
#' @return A matrix.
#' @details fitres() creates a matrix of the fitted values, residuals, and residuals as a proportion (percent) of the actual dependent variable values based on an OLS model or GLM.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitres(model.lm, type = 'response')
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

#####################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE: Generate fitted values and residuals into one matrix.
###
### INPUT:
###   1. lm/glm object. E.g. model.lm <- lm(y ~ x).
###   2. data object.
###   3. type. String.
###
### OUTPUT: matrix.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). fitres.r. https://github.com/robertschnitman/diagnoser
#####################################################################################

fitres <- function(model, type = 'response') {

  ### Type-checking ###
  stopifnot(class(model) == 'lm' | class(model) == 'glm')

  ### Set up fitres matrix ###
  fit             <- predict(model, type = type)
  actual          <- model.frame(model)[, 1]
  residual        <- resid(model)
  residual_margin <- residual/actual

  ### Output ###
  cbind(fit, residual, residual_margin)
}
