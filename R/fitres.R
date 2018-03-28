#' Create matrix of model fit, residuals, and residuals % (proportion).
#'
#' @param model An lm, glm, or nls(model = TRUE) object.
#' @param data Dataframe. If defined, column-wise binds predictions/residuals to dataframe.
#' @param fit_type String. Prediction type. See ?predict for details.
#' @param residual_type String. See ?resid for details.
#' @return A matrix OR dataframe (if data is not null).
#' @details fitres() creates a matrix of the fitted values, residuals, and residuals as a proportion (percent) of the actual dependent variable values based on an OLS model or GLM. If the data input is defined, then said matrix is column-wise binded to the specified dataset.
#' @examples
#' # OLS case
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitres(model.lm, fit_type = 'response')
#' fitres(model.lm, data = mtcars)
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
### PURPOSE: Generate fitted values and residuals into one matrix/dataframe.
###
### OUTPUT: matrix OR data frame.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). fitres.r. https://github.com/robertschnitman/diagnoser
#####################################################################################

fitres <- function(model, data = NULL, fit_type = 'response', residual_type = 'response') {

  ### 1. Type-check model and data inputs. ###
  lgm_condition <- class(model) == 'lm' | class(model)[1] == 'glm'
  nls_condition <- class(model) == 'nls'

  stopifnot(lgm_condition | nls_condition)

  if (any(!'model' %in% names(model))) {

    stop('No model frame exists. If the model input is an nls object, please change it to the following format: nls(y ~ x, data, model = TRUE, ...)')

  }

  if (!is.null(data) & !"data.frame" %in% class(data)) {

    stop('Specified dataset is not a data.frame class (i.e. !"data.frame" %in% class(data).")')

  }

  ### 2. Collect the fit and residuals into a matrix to compare NROWs. ###
  fit             <- predict(model, type = fit_type, na.action = na.exclude)
  actual          <- model.frame(model)[[1]]
  residual        <- if (lgm_condition[1]) {

    resid(model, type = residual_type)

  } else if (nls_condition) {

    r                <- resid(model, type = residual_type)
    attr(r, 'label') <- NULL
    r

  }

  residual_pct <- residual/actual

  fitr <- cbind(fit, residual, residual_pct)

  ### 3. Generate output based on whether the data input is defined. ###

  if (is.null(data)) {

    fitr

  } else {

    cb <- cbind(data, fitr[match(rownames(data), rownames(fitr)), ]) # merge by row-index

    as.data.frame(cb)

  }


}

##### === END === #####
