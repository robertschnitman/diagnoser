#' Graphically diagnose model residuals.
#'
#' @param model An lm, glm, or nls object.
#' @param fit_type String. Default is "response". Type of fitted values to use based on options in predict().
#' @param residual_type String. Default is "response". Type of residuals values to use based on options in resid().
#' @param point_color Color of the points and fill color of the histograms. If left to defaults, fill of the histogram is white.
#' @param line_color Color of the smoothed line.
#' @param pch Point type. Same settings as Base R pch in plot().
#' @param lwd Width of the smoothed line.
#' @return 2x2 charts similar to plot(model.lm): 2 scatter plots and 2 histograms of residuals and residuals %.
#' @examples
#'
#' # OLS case
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' diagnose(model.lm) # defaults
#' diagnose(model.lm, point_color = '#00BFC4', line_color = '#F8766D', pch = 16, lwd = 2)
#'
#' # NLS case
#' model.nls <- nls(Ozone ~ theta0 + Temp^theta1, airquality, model = TRUE)
#' diagnose(model.nls)
#'
#' @section Warning:
#' NLS objects will only work if "model = TRUE" is specified in the original NLS function.
#'
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

######################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE:
###    1. Generate 2x2 graphs that diagnose the residuals of a model.
###    2. Alternative for plot(model.object).
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). diagnose.r. https://github.com/robertschnitman/diagnoser
######################################################################################


##### === BEGIN === #####

diagnose <- function(model, fit_type = 'response', residual_type = 'response',
                     point_color = 'black', line_color = 'black', pch = 1, lwd = 1) {

  ### Type-checking ###
  lgm_condition <- class(model) == 'lm' | class(model)[1] == 'glm'
  nls_condition <- class(model) == 'nls'

  stopifnot(lgm_condition | nls_condition)

  if (any(!'model' %in% names(model))) {

    stop('No model frame exists. If the model input is an nls object, please change it to the following format: nls(y ~ x, data, model = TRUE, ...)')

  }

  ### Set up fitted values, residuals, and 2x2 frame ###
  fit <- predict(model, type = fit_type)
  res <- if (lgm_condition) {
    resid(model, type = residual_type)

  } else if (nls_condition) {
    r                <- resid(model, type = residual_type)
    attr(r, 'label') <- NULL
    r

  }

  act <- model.frame(model)[[1]]
  pct <- (res/act)*100                     # "Margin": residuals as a % of actual values.

  fitr       <- cbind(fit, res, act, pct)
  fitr.noinf <- subset(fitr, !is.infinite(pct))
  fitr.inf   <- subset(fitr, is.infinite(pct))

  warn1      <- paste(NROW(fitr.inf), 'Inf value in residuals margin vector.')
  warn2      <- paste(NROW(fitr.inf), 'Inf values in residuals margin vector.')

  if (NROW(fitr.inf) == 1) {
    warning(warn1)
  } else if (NROW(fitr.inf) > 1) {
    warning(warn2)
  }

  ### Graph is modified based on fit_type and residual_type specifications. ###
  fit_type <- fit_type
  family   <- if (class(model) == 'lm') {
    'lm'
  } else if (class(model)[1] == 'glm') {
    model$family[1]
  } else if (class(model) == 'nls') {
    'nls'
  }

  pp       <- 'Predicted Probabilities'
  fv       <- 'Fitted Values'
  vspp     <- 'vs. Predicted Probabilities'
  vsfv     <- 'vs. Fitted Values'

  ### Set up 2x2 frame ###
  par(mfrow = c(2,2))

  ### Figure 1 - Residuals vs. Fitted ###
  plot(x    = fitr.noinf[ , 'fit'],
       y    = fitr.noinf[ , 'res'],
       xlab = ifelse(fit_type == 'response' & family == 'binomial', pp, fv),
       ylab = 'Residuals',
       main = ifelse(fit_type == 'response' & family == 'binomial',
                     paste('Residuals', vspp, sep = ' '),
                     paste('Residuals', vsfv, sep = ' ')),
       pch  = pch,
       col  = point_color)
  lines(loess.smooth(y = fitr.noinf[ , 'res'], x = fitr.noinf[ , 'fit']), col = line_color, lwd = lwd)
  abline(h = 0, lty = 3)

  ### Figure 2 - Residuals, % vs. Fitted ###
  plot(x    = fitr.noinf[ , 'fit'],
       y    = fitr.noinf[ , 'pct'],
       xlab = ifelse(fit_type == 'response' & family == 'binomial', pp, fv),
       ylab = 'Residuals (%)',
       main = ifelse(fit_type == 'response' & family == 'binomial',
                     paste('Residuals (%)', vspp, sep = ' '),
                     paste('Residuals (%)', vsfv, sep = ' ')),
       pch  = pch,
       col  = point_color)
  lines(loess.smooth(y = fitr.noinf[ , 'pct'], x = fitr.noinf[ , 'fit']), col = line_color, lwd = lwd)
  abline(h = 0, lty = 3)

  ### Figure 3 - Distribution of Residuals ###
  hist(x = res, xlab = 'Residuals', main = 'Distribution of Residuals',
       col = ifelse(point_color == 'black', 'white', point_color))

  ### Figure 4 - Distribution of Residuals (%) ###
  hist(x = pct, xlab = 'Residuals (%)', main = 'Distribution of Residuals (%)',
       col = ifelse(point_color == 'black', 'white', point_color))

  ### Reset mfrow in case user wishes to create other base R plots ###
  par(mfrow = c(1,1))

}
##### === END === #####
