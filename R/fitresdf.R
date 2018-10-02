#' Merge fitted values and residuals to the original data frame.
#'
#' @usage fitresdf(model, data = NULL, fit_type = 'response', residual_type = 'response')
#'
#' @param model An lm, glm, or nls object.
#' @param data A data frame. If unspecified, then the original data is used (i.e. model.frame(model)).
#' @param fit_type String. Default is "response". Type of fitted values to use based on options in predict().
#' @param residual_type String. Default is "response". Type of residuals values to use based on options in resid().
#' @return A data frame. "residual_margin" is the residuals as a proportion of the actual dependent variable values.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitresdf(data = mtcars, model = model.lm)
#'
#' # A warning message displays when there are missing values in the dataset.
#' df       <- mtcars
#' df[1,1]  <- NA
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitresdf(model.lm, data = df)
#'
#' @section Warning:
#' NLS objects will only work if "model = TRUE" is specified in the original NLS function.
#'
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

#####################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE: Generate fitted values and residuals, merging them into a matrix.
###
### OUTPUT: dataframe.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). fitresdf.r. https://github.com/robertschnitman/diagnoser
#####################################################################################

##### === BEGIN === #####

fitresdf <- function(model, data = NULL, fit_type = 'response', residual_type = 'response') {

  ### Type-checking ###
  lgm_condition <- class(model) == 'lm' | class(model)[1] == 'glm'
  nls_condition <- class(model) == 'nls'

  stopifnot(lgm_condition | nls_condition)

  if (any(!'model' %in% names(model))) {

    stop('No model frame exists. If the model input is an nls object, please change it to the following format: nls(y ~ x, data, model = TRUE, ...)')

  }

  ### Collect the fit and residuals into a matrix to compare NROWs ###
  fit             <- predict(model, type = fit_type, na.action = na.exclude)
  actual          <- model.frame(model)[[1]]
  residual        <- if (lgm_condition[1]) {

    resid(model, type = residual_type)

  } else if (nls_condition) {

    r                <- resid(model, type = residual_type)
    attr(r, 'label') <- NULL
    r

  }

  residual_margin <- residual/actual

  fitr <- cbind(fit, residual, residual_margin)

  ### OUTPUT ###

  if (is.null(data)) {

    fitr

  } else {

    cb <- cbind(data, fitr[match(rownames(data), rownames(fitr)), ]) # merge by row-index

    as.data.frame(cb)

  }


}

##### === END === #####
