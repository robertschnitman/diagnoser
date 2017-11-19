#' Create matrix of model fit, residuals, and residuals as a proportion of the fitted values.
#'
#' @param model An lm or glm object.
#' @param data A data frame. If not specified, then the original data is used (i.e. model.frame(model)).
#' @param type String. Prediction type depends on whether the object is lm ('response', 'terms') or glm ('link', 'response', 'terms'). (See ?predict.lm and ?predict.glm for details).
#' @return A matrix.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitres(model.lm)
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

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
###  Schnitman, Robert (2017). fitres.r. https://github.com/robertschnitman/diagnoser
#####################################################################################

fitres <- function(model, data = model.frame(model), type = 'response') {
  fit          <- predict(model, newdata = data, type = type)
  residual     <- resid(model)
  residual_pct <- residual/fit

  cbind(fit, residual, residual_pct)
}
