######################################################################################
### Robert Schnitman
### 2017-11-14
###
### PURPOSE: 
###    1. Generate 2x2 graphs that diagnose the residuals of a model.
###    2. Alternative for plot(model.object).
###
###
### INPUT: lm/glm object. E.g. model.lm <- lm(y ~ x).
### OUTPUT: 2x2 plot with base graphics.
###
### RECOMMENDED CITATION:
###  Schnitman, Robert (2017). diagnose.r. https://github.com/robertschnitman/schnitr
######################################################################################

##### === BEGIN === #####

diagnose <- function(model) {
  fit <- predict(model)
  res <- resid(model)
  pct <- res/fit
  
  par(mfrow = c(2,2))
  
  ### Figure 1 - Residuals vs. Fitted ###
  scatter.smooth(x    = fit, 
                 y    = res, 
                 xlab = 'Fitted Values',
                 ylab = 'Residuals',
                 main = 'Residuals vs. Fitted Values')
  
  ### Figure 2 - Residuals, % vs. Fitted ###
  scatter.smooth(x    = fit, 
                 y    = pct, 
                 xlab = 'Fitted Values',
                 ylab = 'Residuals (proportion)',
                 main = 'Residuals (proportion) vs. Fitted Values')
  
  ### Figure 3 - Distribution of Residuals ###
  hist(x = res, xlab = 'Residuals', main = 'Distribution of Residuals')
  
  ### Figure 4 - Distribution of Residuals, Proportion ###
  hist(x = pct, xlab = 'Residuals (proportion)', main = 'Distribution of Residuals (proportion)')
  
}

##### === END === #####
