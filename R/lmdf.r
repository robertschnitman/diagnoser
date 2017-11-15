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
### INPUTS:
###   1. model = lm object. E.g. model.lm <- lm(y ~ x).
###   2. conf  = Confidence Level. Options are 90, 95, and 99.
###
### OUTPUT: data frame.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). lmdf.r. https://github.com/robertschnitman/schnitr
#####################################################################################

##### === BEGIN === #####

lmdf <- function(model, conf = 95) { # "conf" = Confidence level.
                                         #   Options are 90, 95, and 99.
  
  ### Save model estimates as a data frame. ###
  summary.df <- as.data.frame(summary(model)$coefficients)
  
  ### Replace column names ###
  names(summary.df) <- gsub('Estimate',   'beta', names(summary.df))
  names(summary.df) <- gsub('Std. Error', 'se',   names(summary.df))
  names(summary.df) <- gsub('t value',    't',    names(summary.df))
  names(summary.df) <- gsub('^Pr.*',      'p',    names(summary.df))
  
  ### Generate new variables ###
  summary.df$term     <- rownames(summary.df)                 # Intercept & independent variables.
  
  summary.df$moe      <- with(summary.df,                     # % Margin of Error defined by conf argument.
                              if      (conf == 95) {1.960*se}
                              else if (conf == 90) {1.645*se}
                              else if (conf == 99) {2.576*se}
                              else {stop('Please correctly specify confidence level (conf argument)! \n  Your options are 90, 95, and 99. (b \' v \')b ')}
  )
  
  summary.df$ci_lower <- with(summary.df, beta - moe)         # Confidence Interval: lower.
  summary.df$ci_upper <- with(summary.df, beta + moe)         # Confidence Interval: upper.
  
  ### Remove row names (redundant with term variable) ###
  row.names(summary.df) <- NULL
  
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

## T-statistics & critical value source: Texas A&M University. 2017-11-14.
##   https://www.stat.tamu.edu/~lzhou/stat302/T-Table.pdf
