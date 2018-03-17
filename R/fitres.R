#' Create matrix of model fit, residuals, and residuals margin (residuals as a proportion of the actual values).
#'
#' @param model An lm, glm, or nls(model = TRUE) object.
#' @param fit_type String. Prediction type. See ?predict for details.
#' @param residual_type String. See ?resid for details.
#' @return A matrix.
#' @details fitres() creates a matrix of the fitted values, residuals, and residuals as a proportion (percent) of the actual dependent variable values based on an OLS model or GLM.
#' @examples
#' # OLS case
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitres(model.lm, fit_type = 'response')
#'
#' # NLS case
#' model.nls <- nls(Ozone ~ theta0 + Temp^theta1, airquality, model = TRUE)
#' fitres(model.nls)
#'
#' @section Warning:
#' NLS objects will only work if "model = TRUE" is specified in the original NLS function.
#'
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

#####################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE: Generate fitted values and residuals into one matrix.
###
### OUTPUT: matrix.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). fitres.r. https://github.com/robertschnitman/diagnoser
#####################################################################################

fitres <- function(model, fit_type = 'response', residual_type = 'response') {

  ### Type-checking ###
  lgm_condition <- class(model) == 'lm' | class(model)[1] == 'glm'
  nls_condition <- class(model) == 'nls'

  stopifnot(lgm_condition | nls_condition)

  if (any(!'model' %in% names(model))) {

    stop('No model frame exists. If the model input is an nls object, please change it to the following format: nls(y ~ x, data, model = TRUE, ...)')

  }

  ### Set up fitres matrix ###
  fit             <- predict(model, type = fit_type, na.action = na.include)
  actual          <- model.frame(model)[[1]]

  residual        <- if (lgm_condition[1]) {

    resid(model, type = residual_type)

  } else if (nls_condition) {

    r                <- resid(model, type = residual_type)
    attr(r, 'label') <- NULL
    r

  }

  residual_margin <- residual/actual


  ### Output ###
  cbind(fit, residual, residual_margin)

}
