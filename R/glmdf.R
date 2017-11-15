######################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE:
###   1. Save summary(glm())$coefficients as
###      a formatted data frame.
###   2. Add/rename statistics for model results.
###   3. Improve output as seen in the broom library.
###
### INPUT  = GLM model object. 
###   e.g. model.glm <- glm(y ~ x, family = binomial('logit')).
###
### OUTPUT = data frame.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). glmdf.r. https://github.com/robertschnitman/schnitr
######################################################################################

##### === BEGIN === #####

glmdf <- function(model.glm, conf = 95) { 
  # conf = Confidence level. Options are 90, 95, and 99.
  
  ### set up t/z statistics groups for value replacements in "Replace column names" ###
  t_grp <- c('gaussian', 'Gamma')
  z_grp <- c('binomial', 'poisson')
  
  ### Save model estimates as a data frame. ###
  summary.df <- as.data.frame(summary(model.glm)$coefficients)
  
  ### Replace column names ###
  names(summary.df) <- gsub('Std. Error', 'se', names(summary.df))
  names(summary.df) <- gsub('Estimate', 'beta', names(summary.df))
  
  names(summary.df) <- if (model.glm$family[1] %in% t_grp) {
    gsub('t value', 't', names(summary.df))
  } else if (model.glm$family[1] %in% z_grp) {
    gsub('z value', 'z', names(summary.df))
  } else {
    stop('Invalid family/link. \n  Valid options are gaussian/identity, Gamma/inverse, binomial/logit, and poisson/log. (b \' v \')b ')}
  
  names(summary.df) <- gsub('^Pr.*', 'p', names(summary.df))
  
  ### Generate new variables ###
  summary.df$term      <- rownames(summary.df)                 # Intercept & independent variables.
  
  summary.df$moe        <- with(summary.df,                    # % Margin of Error defined by conf argument.
                                if      (conf == 95) {1.960*se}
                                else if (conf == 90) {1.645*se}
                                else if (conf == 99) {2.576*se}
                                else {stop('Please correctly specify confidence level (conf argument)! \n  Your options are 90, 95, and 99. (b \' v \')b ')}
  )
  
  summary.df$ci_lower   <- with(summary.df, beta - moe)        # Confidence Interval: lower limit.
  summary.df$ci_upper   <- with(summary.df, beta + moe)        # Confidence Interval: upper limit.
  
  ### Remove row names (redundant with term variable) ###
  row.names(summary.df) <- NULL
  
  ### Print reordered columns ###
  if (model.glm$family[1] %in% t_grp) {
  summary.df[, c('term',         # Intercept and independent variables.
                 'beta',         # Coefficients.
                 'se',           # Standard Error
                 'moe',          # % Margin of Error, being specified by conf argument.
                 'ci_lower',     # Lower bound of confidence interval.
                 'ci_upper',     # Upper bound of confidence interval.
                 't',            # T-statistic.
                 'p')]           # p-value.
  } else if (model.glm$family[1] %in% z_grp) {
    summary.df[, c('term',       # Intercept and independent variables.
                   'beta',       # Coefficients.
                   'se',         # Standard Error
                   'moe',        # % Margin of Error, being specified by conf argument.
                   'ci_lower',   # Lower bound of confidence interval.
                   'ci_upper',   # Upper bound of confidence interval.
                   'z',          # z-statistic.
                   'p')]         # p-value.
  } else {
    stop('Invalid family/link. \n  Valid options are gaussian/identity, Gamma/inverse, binomial/logit, and poisson/log. (b \' v \')b ')}
}

##### === END === #####

## z-statistics & critical value source: Armstrong State University. 2017-11-14.
##   http://www.math.armstrong.edu/statsonline/5/5.3.2.html
