#' Merge fitted values and residuals to the original data frame.
#'
#' @param data A data frame. Usually the one used in the model object.
#' @param model An lm or glm object.
#' @return A data frame.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' head(fitresdf(data = mtcars, model = model.lm))
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

#######################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE: Attach fitted values and residuals onto main dataset.
###
### INPUTS:
###    1. data = data frame.
###    2. lm/glm object. E.g. model.lm <- lm(y ~ x).
###
### OUTPUT: data frame.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). fitresdf.r. https://github.com/robertschnitman/diagnoser
#######################################################################################

fitresdf <- function(data, model) {
  data$fit          <- predict(model)
  data$residual     <- resid(model)
  data$residual_pct <- with(data, residual/fit)

  data
}
