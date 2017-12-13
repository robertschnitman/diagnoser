#' Create matrix of model fit, residuals, and residuals margin (residuals as a proportion of the actual values).
#'
#' @param model An lm, glm, or nls(model = TRUE) object.
#' @param data A data frame. If not specified, then the original data is used (i.e. model.frame(model)).
#' @param type String. Prediction type depends on whether the object is lm ('response', 'terms') or glm ('link', 'response', 'terms'). (See ?predict.lm and ?predict.glm for details).
#' @return A matrix.
#' @details fitres() creates a matrix of the fitted values, residuals, and residuals as a proportion (percent) of the actual dependent variable values based on an OLS model or GLM.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitres(model.lm, type = 'response')
#' @section Warning:
#' NLS objects will only work if "model = TRUE" is specified in the original NLS function.
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

#####################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE: Generate fitted values and residuals into one matrix.
###
### INPUT:
###   1. lm/glm or nls object. E.g. model.lm <- lm(y ~ x).
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
  lgm_condition <- class(model) == 'lm' | class(model)[1] == 'glm'
  nls_condition <- class(model) == 'nls'

  stopifnot(lgm_condition | nls_condition)

  ### Set up fitres matrix ###
  fit             <- predict(model, type = type)
  actual          <- if (lgm_condition) {
    model.frame(model)[1]

  } else if (nls_condition) {
    model.frame(model)[[1]]

  }

  residual        <- if (lgm_condition) {
    resid(model)

  } else if (nls_condition) {
    r                <- resid(model)
    attr(r, 'label') <- NULL
    r

  }
  residual_margin <- residual/actual

  ### Output ###
  cbind(fit, residual, residual_margin)

}
