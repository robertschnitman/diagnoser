#' Graphically diagnose model residuals ("classic" version with ggplot2 graphics).
#'
#' @param model An lm or glm object.
#' @param fit_type String. Default is "response". Type of fitted values to use based on options in predict().
#' @param residual_type String. Default is "response". Type of residuals values to use based on options in resid().
#' @param se Boolean. To overlay standard errors.
#' @param alpha Integer, [0, 1]. Points are more transparent the closer they are to 0.
#' @return 2x2 charts that closely resemble the output to plot(model.lm) with ggplot2 graphics.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' cdiagnose(model.lm)
#' @seealso \url{https://github.com/robertschnitman/diagnoser} and
#' Raju Rimal (inspiration for Residuals vs. Leverage): \url{https://rpubs.com/therimalaya/43190}

######################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE:
###    1. Generate 2x2 graphs that diagnose the residuals of a model.
###    2. Alternative for plot(lm()).
###
### LIBRARY DEPENDENCY: ggplot2 (>= 2.2.1), gridExtra (>= 2.3)
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). cdiagnose.r. https://github.com/robertschnitman/diagnoser
###
### Idea for Residuals vs. Leverage inspired by
###  https://rpubs.com/therimalaya/43190.
######################################################################################


##### === BEGIN === #####

cdiagnose <- function(model, fit_type = 'response', residual_type = 'response', se = FALSE, alpha = 1) {
  ### Set alpha value so that ggplot2 functions can process it ###
  a <- alpha

  ### Graph is modified based on fit_type and residual_type specifications. ###
  fit_type <- fit_type
  family   <- ifelse(attr(model, 'class') == 'lm', 'lm', model$family[1]) # To determine whether graph shows fitted values OR predicted probabilities.
  pp       <- 'Predicted Probabilities'
  fv       <- 'Fitted Values'
  vspp     <- 'vs. Predicted Probabilities'
  vsfv     <- 'vs. Fitted Values'

  ### Set up data frame of fit and residuals ###
  fit <- predict(model, type = fit_type)
  res <- resid(model, type = residual_type)
  sr  <- (res - mean(res))/sd(res)            # Standardized Residuals.
  qsr <- qqnorm(sr, plot.it = FALSE)[[1]]     # Theoretical Quantiles; suppress plot.

  lev <- hat(model.matrix(model))             # Leverage.
  cd  <- cooks.distance(model)                # Cook's Distance.

  df  <- as.data.frame(cbind(fit, res, sr))

  ### Figure 1 - Residuals vs. Fitted ###
  f1 <- ggplot(df, aes(y = res, x = fit)) +
    geom_point(color = 'salmon', alpha = a) +
    geom_hline(yintercept = 0,
               col        = 'red',
               linetype   = 'dashed') +
    geom_smooth(method = 'loess',
                se     = se,
                color  = 'steelblue') +
    labs(x     = ifelse(fit_type == 'response' & family == 'binomial', pp, fv),
         y     = 'Residuals',
         title = paste('Residuals',
                       ifelse(fit_type == 'response' & family == 'binomial', vspp, vsfv),
                       sep = ' ')) +
    theme_bw()

  ### Figure 2 - Normal Q-Q Plot ###
  f2 <- ggplot(df, aes(y = sr, x = qsr)) +
    geom_point(color = 'salmon', alpha = a) +
    geom_smooth(method   = 'lm',
                se       = se,
                linetype = 'dashed',
                color    = 'steelblue') +
    labs(x     = 'Theoretical Quantiles',
         y     = 'Standardized Residuals',
         title = 'Normal Q-Q') +
    theme_bw()

  ### Figure 3 - Scale-Location ###
  f3 <- ggplot(df, aes(y = sqrt(abs(sr)), x = fit)) +
    geom_point(color = 'salmon', alpha = a) +
    geom_smooth(method = 'loess',
                se     = se,
                color  = 'steelblue') +
    labs(x     = ifelse(fit_type == 'response' & family == 'binomial', pp, fv),
         y     = expression(sqrt('|Standardized residuals|')),
         title = 'Scale-Location') +
    theme_bw()

  ### Figure 4 - Residuals vs. Leverage ###
  f4 <- ggplot(df, aes(y = sr, x = lev, size = cd)) +
    geom_point(color = 'salmon', alpha = a) +
    geom_smooth(method      = 'loess',
                se          = se,
                color       = 'steelblue',
                show.legend = FALSE
    ) +
    labs(x     = 'Leverage',
         y     = 'Standardized Residuals',
         size  = 'Cook\'s Distance',
         title = 'Residuals vs. Leverage') +
    theme_bw() +
    theme(legend.position = 'bottom')

  grid.arrange(f1, f2, f3, f4, ncol = 2)

}

##### === END === #####
