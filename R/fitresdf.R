#' Merge fitted values and residuals to the original data frame.
#'
#' @param data A data frame. Usually the one used in the model object.
#' @param model An lm or glm object.
#' @return A data frame.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitresdf(data = mtcars, model = model.lm)
#'
#' # A warning message displays when there are missing values in the dataset.
#' df       <- mtcars
#' df[1,1]  <- NA
#' model.lm <- lm(data = df, formula = mpg ~ wt + gear)
#' fitresdf(df, model.lm)
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

#######################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE: Attach fitted values and residuals onto main dataset.
###
### INPUTS:
###    1. data = data frame.
###    2. lm/glm object. E.g. model.lm <- lm(y ~ x).
###
### OUTPUT: data frame.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). fitresdf.r. https://github.com/robertschnitman/diagnoser
#######################################################################################

##### === BEGIN === #####

fitresdf <- function(data, model) {
  ### Collect the fit and residuals into a matrix to compare NROWs ###
  fit          <- predict(model)
  residual     <- resid(model)
  residual_pct <- residual/fit

  fitr <- cbind(fit, residual, residual_pct)

  ### Need to combine the datasets and reinsert rows with missing values from the original dataset. ###
  ## Remember that column names must be the same between datasets for rbind() to work. ##
  if (NROW(data) != NROW(fitr)) {

    data_na              <- data[rowSums(is.na(data)) > 0, ]
    data_na$fit          <- NA
    data_na$residual     <- NA
    data_na$residual_pct <- NA

    data2   <- na.omit(data)           # Need to be mergeable with fitr matrix.

    # Warning message depends on number of rows omitted in data2 #
    diff  <- NROW(data) - NROW(data2)
    warn1 <- 'row with missing values was moved to the bottom of the data frame.'
    warn2 <- 'rows with missing values were moved to the bottom of the data frame.'

    if (diff == 1) {
      warning(paste(diff, warn1, sep = ' '))
    } else {
        warning(paste(diff, warn2, sep = ' '))
      }

    rbind(cbind(data2, fitr), data_na)

  } else {cbind(data, fitr)}
}

##### === END === #####
