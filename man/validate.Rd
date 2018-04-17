#' Common regression fit statistics in a vector.
#'
#' @param model An lm, glm, or nls object.
#' @param dataframe Logical. FALSE (default) outputs a matrix; TRUE outputs a dataframe.
#' @param ... Arguments passed to resid().
#' @return Vector or dataframe. Includes F-statistic, R-squared, RMSE, and others.
#' @details The broom library's glance() had a vague label for the F statistic (simply "statistic") and lacked the pseudo R-squared, which is commonly based on McFadden's version (i.e. 1 - (residual deviance / null deviance)).
#' While the same function is friendly for data frames, it's wide form is cumbersome for quickly ascertaining model validity. Thus, validate() produces similar output as a column vector. Those who wish to have the values in broom's format can always transpose the vector.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' validate(model.lm, TRUE)
#'
#' model.glm <- glm(data = mtcars, am ~ mpg + wt, family = binomial(link = 'logit'))
#' validate(model.glm)
#'
#' model.nls <- nls(Ozone ~ theta0 + Temp^theta1, airquality)
#' validate(model.nls)
#'
#' @section Output definitions (alphabetical order):
#' adj.rsq = Adjusted R-Squared.
#'
#' aer = Apparent Error Rate, calculated as the proportion of misclassifications (i.e. number of incorrect / total cases). Cutoff is the proportion of positive cases.
#'
#' AIC = Akaike Information Criterion.
#'
#' BIC = Bayesian Information Criterion.
#'
#' convergence_tolerance = Tolerance of convergence, calculated from summary(model)$convInfo$finTol
#'
#' df.den = degrees of freedom, denominator.
#'
#' df.null = Degrees of freedom for the null deviance.
#'
#' df.num = degrees of freedom, numerator.
#'
#' df.sigma = degrees of freedom for sigma.
#'
#' F.stat = F statistic
#'
#' iterations = Number of iterations for NLS model to converge.
#'
#' loglik = Log Likelihood.
#'
#' mad = Median Absolute Deviation.
#'
#' mae = Mean Absolute Error.
#'
#' mpe = Mean Percentage Error.
#'
#' medianpe = Median Percentage Error.
#'
#' n = number of observations used in the model.
#'
#' null.deviance = Null Deviance.
#'
#' p.value = p-value for the F statistic.
#'
#' pseudo.rsq.mcfad = McFadden's Pseudo R-Squared, calculated as 1 - (residual.deviance/null.deviance).
#'
#' residual.deviance = Residual Deviance.
#'
#' residual.mean = Mean of the residual.
#'
#' residual.median = Median of the residual.
#'
#' residual.sd = Standard deviation of the residual.
#'
#' residual.se = Standard error of the residual.
#'
#' rmse = Root Mean Square Error, calculated as sqrt(mean(resid(model)^2)).
#'
#' rsq = R-squared.
#'
#' sdpe = Standard Deviation of the Percent Error.
#'
#' sepe = Standard Error of the Percent Error (sd(residuals %)/sqrt(n)).
#'
#' sigma = Standard deviation of the NLS model, calculated from summary(model)$sigma
#'
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

########################################################################################
### Robert Schnitman
### 2017-12-01
###
### PURPOSE: Produce common model statistics such as F, R^2, and RMSE in a vector.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). validate.r. https://github.com/robertschnitman/diagnoser
########################################################################################

##### === BEGIN === #####
validate <- function(model, dataframe = FALSE, ...) {

  ### Type-checking ###
  stopifnot(any(c('lm', 'glm', 'nls') %in% class(model)[1]))

  ### Definitions for different validation statistics ###
  summ     <- summary(model)

  ### Mutual statistics ###
  fit    <- predict(model, type = 'response')
  r      <- resid(model, ...)        # Easier to read when setting up variables.
  actual <- r + fit                  # Residual = actual - fit --> Residual + fit = actual.

  n               <- nobs(model)     #  Exclude from "common" object for ordering purposes.
  residual.median <- median(r)
  residual.mean   <- mean(r)
  residual.sd     <- sd(r)
  residual.se     <- residual.sd/sqrt(n)
  rmse            <- sqrt(mean(r^2))
  mad             <- median(abs(r - median(r)))
  mae             <- mean(abs(r))
  medianpe        <- median(r/actual)
  mpe             <- mean(r/actual)
  sdpe            <- sd(r/actual)
  sepe            <- sdpe/sqrt(n)
  AIC             <- AIC(model)
  BIC             <- BIC(model)
  loglik          <- logLik(model)[1] # [2] is the degrees of freedom.

  common          <- rbind(residual.median, residual.mean, residual.sd, residual.se, rmse, mad, mae, medianpe, mpe, sdpe, sepe, AIC, BIC, loglik)


  ### Case 1: OLS ###
  if (class(model)[1] == 'lm') {
    rsq           <- summ$r.squared
    adj.rsq       <- summ$adj.r.squared

    f             <- summ$fstatistic
    F.stat        <- f[1]
    df.num        <- summ$df[[1]]
    df.den        <- summ$df[[2]]

    p             <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    attributes(p) <- NULL
    p.value       <- p

    ols_stats     <- rbind(rsq, adj.rsq, F.stat, df.num, df.den, p.value)

    output        <- rbind(n, ols_stats, common)

  ### Case 2: GLM ###
  } else if (class(model)[1] == 'glm') {

    null.deviance     <- summ$null.deviance
    df.null           <- summ$df.null
    residual.deviance <- summ$deviance
    df.residual       <- summ$df.residual
    pseudo.rsq.mcfad  <- 1 - (residual.deviance/null.deviance)


    glm_stats         <- rbind(null.deviance, residual.deviance, df.null, df.residual)

    output            <- if (family(model)$link == 'logit') {

      actual_glm <- model.frame(model)[1]
      actual_glm <- if (NROW(unique(actual_glm)) > 2) {

        stop('More than 2 unique values in the dependent variable were detected.
           Please use a multinomial logistic/probit method instead!')

      } else if (all(c(0, 1) != unique(actual_glm[order(actual_glm), ]))) {

        y <- as.numeric(factor(actual_glm))

        y[min(y)] <- 0

        y[max(y)] <- 1

        y

      } else if (all(c(0, 1) == unique(actual_glm[order(actual_glm), ]))) {

        model.frame(model)[1]

      }

      act_table  <- as.data.frame(table(actual_glm)) # For Apparent Error Rate, aer.
      cutoff     <- with(act_table, Freq[actual_glm == 1]/sum(Freq))
      fit_binary <- ifelse(fit <= cutoff, 0, 1)
      aer        <- length(fit_binary[fit_binary != actual_glm])/NROW(actual_glm)

      rbind(n, pseudo.rsq.mcfad, aer, glm_stats, common)

    } else {

      rbind(n, pseudo.rsq.mcfad, glm_stats, common)

    }

  ### Case 3: NLS ###
  } else if (class(model)[1] == 'nls') {

    iterations            <- summ$convInfo$finIter
    convergence_tolerance <- summ$convInfo$finTol
    sigma                 <- summ$sigma
    df.sigma              <- summ$df[2]

    nls_stats             <- rbind(iterations, convergence_tolerance, sigma, df.sigma)

    output                <- rbind(n, nls_stats, common)


  }

  ### OUTPUT ###
  colnames(output) <- deparse(substitute(model))
  output <- round(output, 6) # if not rounded, digits are in form 0.000000e+00.

  value_col <- colnames(output) # The model object name can vary,
                                #   and we need to order the final
                                #   output columns accordingly.

  if (dataframe == FALSE) {

    output # matrix

  } else { # dataframe

    output <- as.data.frame(output)

    output$statistic <- row.names(output)

    row.names(output) <- NULL           # Redundant with the statistic column.

    output[, c("statistic", value_col)] # reverse order so that the category is first


  }


}

##### === END === #####
