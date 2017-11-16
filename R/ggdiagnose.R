#' Graphically diagnose model residuals (ggplot2 version).
#'
#' @param model An lm or glm object.
#' @param bins Number of bins to specify for histograms.
#' @return 2x2 charts similar to plot(model.lm).
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' ggdiagnose(model.lm, bins = NROW(mtcars))
#' @seealso \url{https://github.com/robertschnitman/schnitr}

########################################################################################
### Robert Schnitman
### 2017-11-15
###
### PURPOSE:
###    1. Generate 2x2 graphs that diagnose the residuals of a model.
###    2. Alternative for plot(model.object).
###
### LIBRARY DEPENDENCY: ggplot2 (>= 2.2.1), gridExtra (>= 2.3)
###
### INPUT: lm/glm object. E.g. model.lm <- lm(y ~ x).
### OUTPUT: 2x2 plot with ggplot2 graphics.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). ggdiagnose.r. https://github.com/robertschnitman/schnitr
########################################################################################


##### === BEGIN === #####

ggdiagnose <- function(model, bins = 30) {
  ### Set up data frame of fit and residuals ###
  fit <- predict(model.lm)
  res <- resid(model.lm)
  pct <- (res/fit)*100
  df  <- as.data.frame(cbind(fit, res, pct))

  ### Figure 1 - Residuals vs. Fitted ###
  f1 <- ggplot(df, aes(y = res, x = fit)) +
    geom_point(color = 'salmon') +
    geom_smooth(method = 'loess', se = FALSE, color = 'steelblue') +
    labs(x = 'Fitted Values', y = 'Residuals', title = 'Residuals vs. Fit')

  ### Figure 2 - Residuals, % vs. Fitted ###
  f2 <- ggplot(df, aes(y = pct, x = fit)) +
    geom_point(color = 'salmon') +
    geom_smooth(method = 'loess', se = FALSE, color = 'steelblue') +
    labs(x = 'Fitted Values', y = 'Residuals', title = 'Residuals (%) vs. Fit')

  ### Figure 3 - Distribution of Residuals ###
  f3 <- ggplot(df, aes(x = res)) +
    geom_histogram(bins = bins, fill = 'salmon', colour = 'black') +
    labs(x = 'Residuals', y = 'Frequency', title = 'Distribution of Residuals')

  ### Figure 4 - Distribution of Residuals, Proportion ###
  f4 <- ggplot(df, aes(x = pct)) +
    geom_histogram(bins = bins, fill = 'salmon', colour = 'black') +
    labs(x = 'Residuals (%)', y = 'Frequency', title = 'Distribution of Residuals (%)')

  ### Arrange in 2x2 grid ###
  grid.arrange(f1, f2, f3, f4, ncol = 2)
}

##### === END === #####
