#' Merge fitted values and residuals to the original data frame.
#'
#' @param model An lm or glm object.
#' @param data A data frame. If unspecified, then the original data is used (i.e. model.frame(model)).
#' @param type String. Prediction type depends on whether the object is lm ('response', 'terms') or glm ('link', 'response', 'terms'). (See ?predict.lm and ?predict.glm for details).
#' @return A data frame. "residual_margin" is the residuals as a proportion of the actual dependent variable values.
#' @examples
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitresdf(data = mtcars, model = model.lm)
#'
#' # A warning message displays when there are missing values in the dataset.
#' df       <- mtcars
#' df[1,1]  <- NA
#' model.lm <- lm(data = mtcars, formula = mpg ~ wt + gear)
#' fitresdf(model.lm, data = df)
#' @seealso \url{https://github.com/robertschnitman/diagnoser}

##### === BEGIN === #####

fitresdf <- function(model, data, type = 'response') {

  ### Type-checking ###
  if (!is.object(model)) {
    stop('Please use an lm or glm object for the "model" input!')
  } else if (!is.data.frame(model) | !is.matrix(model)) {
    stop('Please use a matrix or dataframe for the "data" input!')
  }


  ### Collect the fit and residuals into a matrix to compare NROWs ###
  fit             <- predict(model, type = type)
  actual          <- model.frame(model)[, 1]
  residual        <- resid(model)
  residual_margin <- residual/actual

  fitr <- cbind(fit, residual, residual_margin)

  ### Need to combine the datasets and reinsert rows with missing values from the original dataset. ###
  ## Remember that column names must be the same between datasets for rbind() to work. ##
  if (NROW(data) != NROW(fitr)) {

    data_na <- data[rowSums(is.na(data)) > 0, ]
    data_na <- transform(data_na, fit = NA, residual = NA, residual_margin = NA)

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
