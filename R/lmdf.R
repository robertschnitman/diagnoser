#' Transform lm() object into a data frame with margin of error and confidence intervals.
#'
#' @param model An lm object.
#' @param conf A confidence level, 0-1. Consistent with the level argument in confint().
#' @return A data frame.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' lmdf(model = model.lm, conf = 0.90)
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

#####################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE:
###   1. Save summary(lm())$coefficients as
###      a formatted data frame.
###   2. Add/rename statistics for model results.
###   3. Improve output from the broom library.
###
### OUTPUT: data frame.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). lmdf.r. https://github.com/robertschnitman/diagnoser
#####################################################################################

##### === BEGIN === #####

lmdf <- function(model, conf = 0.95) { # conf for confint

  ### model should be an object and conf argument should be within 0-1 ###
  stopifnot(is.object(model), conf >= 0 & conf <= 1)

  ### Save model estimates as a data frame. ###
  summary.df <- as.data.frame(summary(model)$coefficients)

  ### Replace column names ###
  names(summary.df) <- gsub('Estimate',   'beta', names(summary.df))
  names(summary.df) <- gsub('Std. Error', 'se',   names(summary.df))
  names(summary.df) <- gsub('t value',    't',    names(summary.df))
  names(summary.df) <- gsub('^Pr.*',      'p',    names(summary.df))

  ### Generate new variables ###
  summary.df$term     <- rownames(summary.df)                 # Intercept & independent variables.

  ci                  <- confint(model, level = conf)                       # To set up ci columns
  summary.df$ci_lower <- t(t(ci[rownames(ci) == rownames(summary.df), 1]))  # Confidence Interval: lower.
  summary.df$ci_upper <- t(t(ci[rownames(ci) == rownames(summary.df), 2]))  # Confidence Interval: upper.

  summary.df$moe      <- with(summary.df, ci_upper - beta)


  ### Remove row names (redundant with term variable) ###
  rownames(summary.df) <- NULL

  ### Print reordered columns ###
  summary.df[, c('term',         # Intercept and independent variables.
                 'beta',         # Coefficients.
                 'se',           # Standard Error
                 'moe',          # % Margin of Error, being specified by conf argument.
                 'ci_lower',     # Lower bound of confidence interval.
                 'ci_upper',     # Upper bound of confidence interval.
                 't',            # T-statistic.
                 'p')]           # p-value.

}

##### === END === #####
