#' Common regression fit statistics in a vector.
#'
#' @param model An lm, glm, or nls object.
#' @return Vector (column). Includes F-statistic, R-squared, RMSE, and others.
#' @details The broom library's glance() had a vague label for the F statistic (simply "statistic") and lacked the pseudo R-squared, which is commonly based on McFadden's version (i.e. 1 - (residual deviance / null deviance)).
#' While the same function is friendly for data frames, it's wide form is cumbersome for quickly ascertaining model validity. Thus, validate() produces similar output as a column vector. Those who wish to have the values in broom's format can always transpose the vector.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' validate(model.lm)
#'
#' model.glm <- glm(data = mtcars, am ~ mpg + wt, family = binomial(link = 'logit'))
#' validate(model.glm)
#'
#' require(graphics)
#' DNase1    <- subset(DNase, Run == 1)
#' model.nls <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1, model = TRUE)
#' validate(model.nls)
#'
#' @section Output definitions:
#' adj.rsq = Adjusted R-Squared
#' AIC = Akaike Information Criterion.
#' BIC = Bayesian Information Criterion.
#' convergence_tolerance = Tolerance of convergence, calculated from summary(model)$convInfo$finTol
#' df.den = degrees of freedom, denominator.
#' df.null = Degrees of freedom for the null deviance.
#' df.num = degrees of freedom, numerator.
#' df.sigma = degrees of freedom for sigma.
#' error_rate = (Apparent) Error Rate, calculated as number of misclassifications divided by correct classifications.
#' F.stat = F statistic
#' iterations = Number of iterations for NLS model to converge.
#' loglik = Log Likelihood.
#' mae = Mean Absolute Error.
#' mpe = Mean Percentage Error.
#' n = number of observations used in the model.
#' null.deviance = Null Deviance.
#' p.value = p-value for the F statistic.
#' pseudo.rsq.mcfad = McFadden's Pseudo R-Squared, calculated as 1 - (residual.deviance/null.deviance).
#' residual.deviance = Residual Deviance.
#' residual.mean = mean of the residual.
#' residual.median = median of the residual.
#' residual.sd = Standard deviation of the residual.
#' rmse = Root Mean Square Error, calculated as sqrt(mean(resid(model)^2)).
#' rsq = R-squared.
#' sigma = Standard deviation of the NLS model, calculated from summary(model)$sigma
#'
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

########################################################################################
### Robert Schnitman
### 2017-12-01
###
### PURPOSE: Common model statistics such as F, R^2, and RMSE in a vector.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). validate.r. https://github.com/robertschnitman/diagnoser
########################################################################################

##### === BEGIN === #####
validate <- function(model) {

  ### Type-checking ###
  stopifnot(any(c('lm', 'glm', 'nls') %in% class(model)[1]))

  ### Definitions for different validation statistics ###
  summ     <- summary(model)

  ### Mutual statistics ###
  n               <- nobs(model)           # Exclude from "common" object for ordering purposes.
  residual.median <- median(resid(model))
  residual.mean   <- mean(resid(model))
  residual.sd     <- sd(resid(model))
  rmse            <- sqrt(mean(resid(model)^2))
  mae             <- mean(abs(resid(model)))
  mpe             <- mean(resid(model)/model.frame(model)[[1]])
  AIC             <- AIC(model)
  BIC             <- BIC(model)
  loglik          <- logLik(model)[1]

  common          <- rbind(residual.median, residual.mean, residual.sd, rmse, mae, mpe, AIC, BIC, loglik)


  ### Case 1: OLS ###
  if (class(model)[1] == 'lm') {
    rsq           <- summ$r.squared
    adj.rsq       <- summ$adj.r.squared

    f             <- summ$fstatistic
    F.stat         <- f[1]
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

    aer               <- function(y, yhat) {

      length(yhat[yhat != y])/length(yhat[yhat == y])

    } # Apparent Error Rate

    fit               <- predict(model, type = 'response')
    actual            <- model.frame(model)[[1]]
    fit_binary        <- ifelse(fit < 0.5, 0, 1)
    error_rate        <- aer(actual, fit_binary)


    glm_stats         <- rbind(null.deviance, residual.deviance, df.null, df.residual)

    output            <- if (family(model)$link == 'logit') {
      rbind(n, pseudo.rsq.mcfad, error_rate, glm_stats, common)
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
  round(output, 6) # if not rounded, digits are in form 0.000000e+00.

}

##### === END === #####
