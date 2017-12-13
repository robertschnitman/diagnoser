#' Create matrix of model fit, residuals, and residuals margin (residuals as a proportion of the actual values).
#'
#' @param model An lm, glm, or nls(model = TRUE) object.
#' @param data A data frame. If not specified, then the original data is used (i.e. model.frame(model)).
#' @param fit_type String. Prediction type. See ?predict for details.
#' @param residual_type String. See ?resid for details.
#' @return A matrix.
#' @details fitres() creates a matrix of the fitted values, residuals, and residuals as a proportion (percent) of the actual dependent variable values based on an OLS model or GLM.
#' @examples
#' # OLS case
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitres(model.lm, type = 'response')
#'
#' # NLS case
#' require(graphics)
#' DNase1    <- subset(DNase, Run == 1)
#' fm1DNase1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
#' fitres(fm1DNase1)
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

  ### Set up fitres matrix ###
  fit             <- predict(model, type = fit_type)
  actual          <- if (lgm_condition) {
    model.frame(model)[1]

  } else if (nls_condition) {
    model.frame(model)[[1]]

  }

  residual        <- if (lgm_condition) {
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
