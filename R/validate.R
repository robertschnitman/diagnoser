#' Common model statistics in a vector.
#'
#' @param model An lm or glm object.
#' @return Vector (column). Includes F-statistic, R-squared, RMSE, and others.
#' @details The broom library's glance() had a vague label for the F statistic (simply "statistic") and lacked the pseudo R-squared, which is commonly based on McFadden's version (i.e. 1 - (residual deviance / null deviance)).
#' While the same function is friendly for data frames, it's wide form is cumbersome for quickly ascertaining model validity. Thus, validate() produces similar output as a column vector. Those who wish to have the values in broom's format can always transpose the vector.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' validate(model.lm)
#'
#' model.glm <- glm(data = mtcars, am ~ mpg + wt, family = binomial(link = 'logit'))
#' validate(model.glm)
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

  ### Definitions for different validation statistics ###
  summ     <- summary(model)
  family   <- ifelse(attr(model, 'class')[[1]] == 'lm',
                     'lm',
                     model$family[1])

  ### Common statistics for LM and GLM ###
  median.residual <- median(resid(model))
  mean.residual   <- mean(resid(model))
  sd.residual     <- sd(resid(model))
  rmse            <- sqrt(mean(resid(model)^2))
  AIC             <- AIC(model)
  BIC             <- BIC(model)
  logLik          <- logLik(model)[1]

  common          <- cbind(median.residual, mean.residual, sd.residual,
                           rmse, AIC, BIC,
                           logLik)

  ### Case 1: OLS ###
  if (family == 'lm') {
    rsq     <- summ$r.squared
    adj.rsq <- summ$adj.r.squared

    f       <- summ$fstatistic
    Fstat   <- f[1]
    dfnum   <- summ$df[[1]]
    dfden   <- summ$df[[2]]

    p             <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    attributes(p) <- NULL
    p.value       <- p

    validation    <- t(cbind(rsq, adj.rsq, common,
                             Fstat, dfnum, dfden,
                             p.value))

  ### Case 2: GLM ###
  } else {
    coefs <- NROW(coef(model))

    null.deviance     <- summ$null.deviance
    df.null           <- summ$df.null
    residual.deviance <- summ$deviance
    df.residual       <- summ$df.residual
    pseudo.rsq.mcfad  <- 1- (residual.deviance/null.deviance)
    adj.p.rsq.mcfad   <- 1- ((residual.deviance - coefs)/null.deviance)

    validation           <- t(cbind(pseudo.rsq.mcfad, adj.p.rsq.mcfad,
                                    null.deviance, residual.deviance,
                                    df.null, df.residual, common))
    colnames(validation) <- 'value' # "value" auto-inputted for lm case for some reason...

  }

  ### OUTPUT ###
  validation  # Let values float: if needed, use round().
}

##### === END === #####
