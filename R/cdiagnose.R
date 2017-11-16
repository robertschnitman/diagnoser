#' Graphically diagnose model residuals ("classic" version with ggplot2 graphics).
#'
#' @param model An lm or glm object.
#' @param se Boolean. For overlaying standard errors.
#' @return 2x2 charts similar to plot(model.lm).
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
### INPUT: lm/glm object. E.g. model.lm <- lm(y ~ x).
### OUTPUT: 2x2 plot with base graphics.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). diagnose.r. https://github.com/robertschnitman/diagnoser
###
### Idea for Residuals vs. Leverage inspired by
###  https://rpubs.com/therimalaya/43190.
######################################################################################


##### === BEGIN === #####

cdiagnose <- function(model, se = FALSE) {

  ### Set up data frame of fit and residuals ###
  fit <- predict(model)
  res <- resid(model)
  sr  <- (res - mean(res))/sd(res)            # Standardized Residuals.
  qsr <- qqnorm(sr, plot.it = FALSE)[[1]]     # Theoretical Quantiles; suppress plot.

  lev <- hat(model.matrix(model))             # Leverage.
  cd  <- cooks.distance(model)                # Cook's Distance.

  df  <- as.data.frame(cbind(fit, res, sr))

  ### Figure 1 - Residuals vs. Fitted ###
  f1 <- ggplot(df, aes(y = res, x = fit)) +
    geom_point(color = 'salmon') +
    geom_hline(yintercept = 0,
               col        = 'red',
               linetype   = 'dashed') +
    geom_smooth(method = 'loess',
                se     = se,
                color  = 'steelblue') +
    labs(x     = 'Fitted Values',
         y     = 'Residuals',
         title = 'Residuals vs. Fit') +
    theme_bw()

  ### Figure 2 - Normal Q-Q Plot ###
  f2 <- ggplot(df, aes(y = sr, x = qsr)) +
    geom_point(color = 'salmon') +
    geom_smooth(method   = 'lm',
                se       = se,
                linetype = 'dashed',
                color    = 'steelblue') +
    labs(x     = 'Theoretical Quantiles',
         y     = 'Standardized Residuals',
         title = 'Normal Q-Q') +
    theme_bw()

  ### Figure 3 - Scale-Location ###
  f3 <- ggplot(df, aes(y = sqrt(sr), x = fit)) +
    geom_point(color = 'salmon') +
    geom_smooth(method = 'loess',
                se     = se,
                color  = 'steelblue') +
    labs(x     = 'Fitted Values',
         y     = expression(sqrt('|Standardized residuals|')),
         title = 'Scale-Location') +
    theme_bw()

  ### Figure 4 - Residuals vs. Leverage ###
  f4 <- ggplot(df, aes(y = sr, x = lev, size = cd)) +
    geom_point(color        = 'salmon') +
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
