#' Graphically diagnose model residuals (ggplot2 version).
#'
#' @param model An lm or glm object.
#' @param fit_type String. Default is "response". Type of fitted values to use based on options in predict().
#' @param residual_type String. Default is "response". Type of residuals values to use based on options in resid().
#' @param bins Number of bins to specify for histograms.
#' @param se Boolean. For overlaying shaded standard errors.
#' @param freqpct Boolean.
#' @param alpha Integer, [0, 1]. Points are more transparent the closer they are to 0. Only applies to scatter plots.
#' @return 2 scatter plots and 2 histograms of residuals and "residuals margin,"
#' which is the residuals as a percentage of the actual dependent variable values.
#' @examples
#'
#' # OLS case
#' model <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' ggdiagnose(model, bins = NROW(mtcars), se = FALSE, freqpct = TRUE)
#'
#' # NLS case
#' require(graphics)
#' DNase1    <- subset(DNase, Run == 1)
#' fm1DNase1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1, model = TRUE)
#' diagnose(fm1DNase1)
#'
#' @section Warning:
#' NLS objects will only work if "model = TRUE" is specified in the original NLS function.
#'
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

########################################################################################
### Robert Schnitman
### 2017-11-15
###
### PURPOSE:
###    1. Generate 2x2 graphs that diagnose the residuals of a model.
###    2. Alternative for plot(model.object).
###
### IMPORTS: ggplot2 (>= 2.2.1), gridExtra (>= 2.3)
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). ggdiagnose.r. https://github.com/robertschnitman/diagnoser
########################################################################################


##### === BEGIN === #####

ggdiagnose <- function(model, fit_type = 'response', residual_type = 'response',
                       bins = 30, se = TRUE, freqpct = FALSE,
                       alpha = 1) {

  ### Type-checking ###
  lgm_condition <- class(model) == 'lm' | class(model)[1] == 'glm'
  nls_condition <- class(model) == 'nls'

  stopifnot(lgm_condition | nls_condition)

  options(warn = -1)

  if (require(ggplot2) == TRUE & require(gridExtra) == TRUE) {
    require(ggplot2)
    require(gridExtra)
  } else {

    stop('Please install both ggplot2 and gridExtra.')
  }

  options(warn = 1)

  ### Set alpha value so that ggplot2 functions can process it ###
  a <- alpha

  ### Graph is modified based on fit_type and residual_type specifications. ###
  fit_type <- fit_type
  family   <-  if (class(model) == 'lm') {
    'lm'
  } else if (class(model)[1] == 'glm') {
    model$family[1]
  } else if (class(fm1DNase1) == 'nls') {
    'nls'
  }
  pp       <- 'Predicted Probabilities'
  fv       <- 'Fitted Values'
  vspp     <- 'vs. Predicted Probabilities'
  vsfv     <- 'vs. Fitted Values'

  ### Set up data frame of fit and residuals ###
  fit <- predict(model, type = fit_type)
  res <- if (lgm_condition) {
    resid(model, type = residual_type)

  } else if (nls_condition) {
    r                <- resid(model, residual_type)
    attr(r, 'label') <- NULL
    r

  }
  act <- model.frame(model)[[1]]

  pct <- (res/act)
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
      labs(x     = ifelse(fit_type == 'response' & family == 'binomial', pp, fv),
           y     = ylabel,
           title = paste(ylabel,
                         ifelse(fit_type == 'response' & family == 'binomial', vspp, vsfv), sep = ' ')) +
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
  f1 <- rvf(y = res, x = fit, ylabel = 'Residuals')          # Figure 1 - Residuals vs. Fitted.
  f2 <- rvf(y = pct, x = fit, ylabel = 'Residuals Margin') + # Figure 2 - Residuals Margin (%) vs. Fitted.
    scale_y_continuous(labels = scales::percent)
  f3 <- histres(x = res, xlabel = 'Residuals')               # Figure 3 - Distribution of Residuals.
  f4 <- histres(x = pct, xlabel = 'Residuals Margin') +      # Figure 4 - Distribution of Residuals Margin (%).
    scale_x_continuous(labels = scales::percent)

  ### Arrange in 2x2 grid ###
  grid.arrange(f1, f2, f3, f4, ncol = 2)

}

##### === END === #####
