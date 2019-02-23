#' Transform lm, glm, or nls object into a data frame with margin of error and confidence intervals.
#'
#' @usage modeldf(model, conf = 0.95)
#'
#' @param model An lm, glm, or nls model object.
#' @param conf A confidence level, 0-1.
#' @return A data frame.
#' @examples
#' model.glm <- glm(data = mtcars, formula = am ~ mpg + gear, family = binomial(link = 'logit'))
#' modeldf(model = model.glm, conf = 0.90)
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

  ### 1. Model should be glm and conf argument should be within 0-1 ###
  stopifnot(any(c('lm', 'glm', 'nls') %in% class(model)[1]), conf >= 0 & conf <= 1)

  ### 2. Verify that the user has the car library installed for vif() ###
  if (!require(car)) {

    stop('Please install the car library so that the VIFs may be computed.')

  } else {

    require(car)

  }

  ### 3. Set up t/z statistics groups for value replacements in "Replace column names" ###
  t_grp <- c('gaussian', 'Gamma')
  z_grp <- c('binomial', 'poisson')

  ### 4. Save model estimates as a data frame. ###
  summary.df <- as.data.frame(summary(model)$coefficients)

  ### 5. Replace column names ###
  names(summary.df) <- gsub('Estimate', 'coef', names(summary.df))

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

  ### 6. Generate new variables (non-VIF) ###
  summary.df$term     <- rownames(summary.df)                               # Intercept & independent variables.

  ci                  <- suppressMessages(confint(model, level = conf))     # To set up ci columns; eliminate profile message.
  summary.df$ci_lower <- t(t(ci[rownames(ci) == rownames(summary.df), 1]))  # Confidence Interval: lower.
  summary.df$ci_upper <- t(t(ci[rownames(ci) == rownames(summary.df), 2]))  # Confidence Interval: upper.


  summary.df$moe      <- with(summary.df, ci_upper - coef)

  ### 7. VIF only works for OLS and GLM ###

  if (any(c('lm', 'glm') %in% class(model)[1])) {

    vifs           <- as.data.frame(t(t(car::vif(model))))
    names(vifs)    <- 'vif'
    vifs$term      <- rownames(vifs)

    summary_vif.df <- merge(summary.df, vifs, by = 'term', all.x = TRUE)

  }

  ### 8. Remove row names (redundant with term variable) ###

  if (any(c('lm', 'glm') %in% class(model)[1])) {

    rownames(summary_vif.df) <- NULL

  } else {

    rownames(summary.df) <- NULL

  }


  ### 9. Print reordered columns ###
  cols_common <- c('coef', 'se', 'moe', 'ci_lower', 'ci_upper')
  cols_t      <- c('t', 'p')
  cols_z      <- c('z', 'p')

  if (class(model)[1] == 'nls') {

    names(summary.df) <- gsub('term', 'parameter', names(summary.df))

    summary.df[, c('parameter', cols_common, cols_t)]

  } else if (class(model)[1] == 'lm' || (class(model)[1] == 'glm' & model$family[1] %in% t_grp)) {

    summary_vif.df[, c('term', cols_common, cols_t, 'vif')]

  } else if (class(model)[1] != 'lm' & class(model)[1] == 'glm' & model$family[1] %in% z_grp) {

    summary_vif.df[, c('term', cols_common, cols_z, 'vif')]

  }

}

##### === END === #####
