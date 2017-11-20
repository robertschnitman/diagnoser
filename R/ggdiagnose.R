#' Graphically diagnose model residuals (ggplot2 version).
#'
#' @param model An lm or glm object.
#' @param bins Number of bins to specify for histograms.
#' @param se Boolean. For overlaying shaded standard errors.
#' @param freqpct Boolean.
#' @param alpha Integer, [0, 1]. Points are more transparent the closer they are to 0. Only applies to scatter plots.
#' @return 2x2 charts similar to plot(model): 2 scatter plots and 2 histograms.
#' @examples
#' model <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' ggdiagnose(model, bins = NROW(mtcars), se = FALSE, freqpct = TRUE)
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

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
### INPUT: lm/glm object. E.g. model <- lm(y ~ x).
### OUTPUT: 2x2 plot with ggplot2 graphics.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). ggdiagnose.r. https://github.com/robertschnitman/diagnoser
########################################################################################


##### === BEGIN === #####

ggdiagnose <- function(model, bins = 30, se = TRUE, freqpct = FALSE, alpha = 1) {
  ### Set alpha value so that ggplot2 functions can process it ###
  a <- alpha

  ### Set up data frame of fit and residuals ###
  fit <- predict(model)
  res <- resid(model)
  act <- model.frame(model)[, 1]
  pct <- (res/act)*100
  df  <- as.data.frame(cbind(fit, res, pct))

  ### ggplot2 graphs use the same functions/colors; need to minimize repeating code ###

  ## Residuals vs. fitted values ##
  rvf <- function(y, x, ylabel = 'yvar') {
    ggplot(df, aes(y = y, x = x)) +
      geom_point(color = 'salmon', alpha = a) +
      geom_hline(yintercept = 0,
                 col        = 'red',
                 linetype   = 'dashed') +
      geom_smooth(method = 'loess',
                  se     = se,
                  color  = 'steelblue') +
      labs(x     = 'Fitted Values',
           y     = ylabel,
           title = paste(ylabel, 'vs. Fit', sep = ' ')) +
      theme_bw()
  }

  ## Histogram of residuals ##
  histres <- function(x, xlabel, fpct = freqpct) {
    if (fpct == FALSE) {
      ggplot(df, aes(x = x)) +
        geom_histogram(bins   = bins,
                       fill   = 'salmon',
                       colour = 'black') +
        labs(x     = xlabel,
             y     = 'Frequency',
             title = paste('Distribution of', xlabel, sep = ' ')) +
        theme_bw()
    } else {
      ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = ..count../sum(..count..)),
                       bins   = bins,
                       fill   = 'salmon',
                       colour = 'black') +
        scale_y_continuous(labels = scales::percent) +
        labs(x     = xlabel,
             y     = 'Frequency (%)',
             title = paste('Distribution of', xlabel, sep = ' ')) +
        theme_bw()
    }

  }

  ### grid.arrange() requires each of the graphs to be created beforehand ###
  f1 <- rvf(y = res, x = fit, ylabel = 'Residuals')            # Figure 1 - Residuals vs. Fitted.
  f2 <- rvf(y = pct, x = fit, ylabel = 'Residuals Margin (%)') # Figure 2 - Residuals Margin (%) vs. Fitted.
  f3 <- histres(x = res, xlabel = 'Residuals')                 # Figure 3 - Distribution of Residuals.
  f4 <- histres(x = res, xlabel = 'Residuals Margin (%)')      # Figure 4 - Distribution of Residuals Margin (%).

  ### Arrange in 2x2 grid ###
  grid.arrange(f1, f2, f3, f4, ncol = 2)

}

##### === END === #####
