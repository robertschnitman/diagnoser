#' Transform lm, glm, or nls object into a data frame with margin of error and confidence intervals.
#'
#' @param model An lm, glm, or nls model object.
#' @param conf A confidence level, 0-1.
#' @return A data frame.
#' @examples
#' model.glm <- glm(data = mtcars, formula = am ~ mpg + gear, family = binomial(link = 'logit'))
#' glmdf(model = model.glm, conf = 0.90)
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

######################################################################################
### Robert Schnitman
### 2017-12-15
###
### PURPOSE:
###   1. Save summary()$coefficients as a formatted data frame.
###   2. Add/rename statistics for model results.
###   3. Improve output as seen in the broom library.
###
### OUTPUT = data frame.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). modeldf.r. https://github.com/robertschnitman/diagnoser
######################################################################################

##### === BEGIN === #####

modeldf <- function(model, conf = 0.95) {

  ### Model should be glm and conf argument should be within 0-1 ###
  stopifnot(any(c('lm', 'glm', 'nls') %in% class(model)[1]), conf >= 0 & conf <= 1)

  ### Set up t/z statistics groups for value replacements in "Replace column names" ###
  t_grp <- c('gaussian', 'Gamma')
  z_grp <- c('binomial', 'poisson')

  ### Save model estimates as a data frame. ###
  summary.df <- as.data.frame(summary(model)$coefficients)

  ### Replace column names ###
  names(summary.df) <- gsub('Estimate',   'beta', names(summary.df))
  names(summary.df) <- gsub('Std. Error', 'se',   names(summary.df))

  names(summary.df) <- if (class(model) == 'lm' | class(model) == 'nls' ||
                           model$family[1] %in% t_grp) {
      gsub('t value', 't', names(summary.df))
    } else if (class(model)[1] == 'glm' & model$family[1] %in% z_grp) {
      gsub('z value', 'z', names(summary.df))
    } else {
      stop('Invalid family/link. \n  Valid options are gaussian/identity, Gamma/inverse, binomial/logit, and poisson/log. (b \' v \')b ')
  }

  names(summary.df) <- gsub('^Pr.*', 'p', names(summary.df))

  ### Generate new variables ###
  summary.df$term     <- rownames(summary.df)                               # Intercept & independent variables.

  ci                  <- suppressMessages(confint(model, level = conf))     # To set up ci columns; eliminate profile message.
  summary.df$ci_lower <- t(t(ci[rownames(ci) == rownames(summary.df), 1]))  # Confidence Interval: lower.
  summary.df$ci_upper <- t(t(ci[rownames(ci) == rownames(summary.df), 2]))  # Confidence Interval: upper.

  summary.df$moe      <- with(summary.df, ci_upper - beta)

  ### Remove row names (redundant with term variable) ###
  rownames(summary.df) <- NULL

  ### Print reordered columns ###
  if (any(c('lm', 'nls') == class(model)[1]) || (class(model)[1] == 'glm' & model$family[1] %in% t_grp)) {
    summary.df[, c('term',         # Intercept and independent variables.
                   'beta',         # Coefficients.
                   'se',           # Standard Error
                   'moe',          # % Margin of Error, being specified by conf argument.
                   'ci_lower',     # Lower bound of confidence interval.
                   'ci_upper',     # Upper bound of confidence interval.
                   't',            # T-statistic.
                   'p')]           # p-value.

  } else if (class(model)[1] == 'glm' & model$family[1] %in% z_grp) {
    summary.df[, c('term',         # Intercept and independent variables.
                   'beta',         # Coefficients.
                   'se',           # Standard Error
                   'moe',          # % Margin of Error, being specified by conf argument.
                   'ci_lower',     # Lower bound of confidence interval.
                   'ci_upper',     # Upper bound of confidence interval.
                   'z',            # z-statistic.
                   'p')]           # p-value.
  }

}

##### === END === #####