#' Graphically diagnose model residuals.
#'
#' @param model An lm or glm object.
#' @param fit_type String. Default is "response". Type of fitted values to use based on options in predict().
#' @param residual_type String. Default is "response". Type of residuals values to use based on options in resid().
#' @return 2x2 charts similar to plot(model.lm): 2 scatter plots and 2 histograms of residuals and "residuals margin,"
#' which is the residuals as a percentage of the actual dependent variable values.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' diagnose(model.lm)
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

diagnose <- function(model, fit_type = 'response', residual_type = 'response') {
  ### Set up fitted values, residuals, and 2x2 frame ###
  fit <- predict(model, type = fit_type)
  res <- resid(model, type = residual_type)
  act <- model.frame(model)[, 1]           # Actual values from the depdendent variable.
  pct <- (res/act)*100                     # "Margin": residuals as a % of actual values.

  fitr       <- cbind(fit, res, act, pct)
  fitr.noinf <- subset(fitr, !is.infinite(pct))
  diff       <- NROW(fitr) - NROW(fitr.noinf)

  warn1      <- 'row was deleted due to infinite values.'
  warn2      <- ' rows were deleted due to infinite values.'
  if (diff == 1) {
    warning(diff, warn1, sep = ' ')
  } else if (diff > 1) {
    warning(diff, warn2, sep = ' ')
  }

  ### Graph is modified based on fit_type and residual_type specifications. ###
  fit_type <- fit_type
  family   <- ifelse(attr(model, 'class') == 'lm', 'lm', model$family[1]) # To determine whether graph shows fitted values OR predicted probabilities.
  pp       <- 'Predicted Probabilities'
  fv       <- 'Fitted Values'
  vspp     <- 'vs. Predicted Probabilities'
  vsfv     <- 'vs. Fitted Values'

  ### Set up 2x2 frame ###
  par(mfrow = c(2,2))

  ### Figure 1 - Residuals vs. Fitted ###
  scatter.smooth(x    = fitr.noinf[ , 'fit'],
                 y    = fitr.noinf[ , 'res'],
                 xlab = ifelse(fit_type == 'response' & family == 'binomial', pp, fv),
                 ylab = 'Residuals',
                 main = ifelse(fit_type == 'response' & family == 'binomial',
                               paste('Residuals', vspp, sep = ' '),
                               paste('Residuals', vsfv, sep = ' ')))

  ### Figure 2 - Residuals, % vs. Fitted ###
  scatter.smooth(x    = fitr.noinf[ , 'fit'],
                 y    = fitr.noinf[ , 'pct'],
                 xlab = ifelse(fit_type == 'response' & family == 'binomial', pp, fv),
                 ylab = 'Residuals Margin (%)',
                 main = ifelse(fit_type == 'response' & family == 'binomial',
                               paste('Residuals Margin (%)', vspp, sep = ' '),
                               paste('Residuals Margin (%)', vsfv, sep = ' ')))

  ### Figure 3 - Distribution of Residuals ###
  hist(x = res, xlab = 'Residuals',            main = 'Distribution of Residuals')

  ### Figure 4 - Distribution of Residuals (%) ###
  hist(x = pct, xlab = 'Residuals Margin (%)', main = 'Distribution of Residuals Margin (%)')

}
##### === END === #####
