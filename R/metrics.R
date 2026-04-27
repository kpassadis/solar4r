validate_metrics <- function(y, yhat) {
  if(length(y) == 0 | length(yhat) == 0) {
    stop("Non empty vectors must be passed for metric calculation")
  }
  if(length(y)  != length(yhat)) {
    stop("y and yhat must have the same length")
  }
  if (!is.numeric(y) | !is.numeric(yhat)) {
    stop("Expecting y and yhat to be numeric vectors")
  }
}

#' Calculates the Root Mean Squared Error (RMSE)
#' 
#' @param y A numeric vector of target values (truth).
#' @param yhat A numeric vector of predicted values (estimates).
#' @returns A numeric value in the same units as y
#' @export
rmse <- function(y, yhat) {
  validate_metrics(y, yhat)
  sqrt(mean((y - yhat)^2, na.rm = TRUE))
}

#' Calculates the Mean Absolute Error (MAE)
#' 
#' @param y A numeric vector of target values.
#' @param yhat A numeric vector of predicted values.
#' @returns A numeric value 
#' @export
mae <- function(y, yhat) {
  validate_metrics(y, yhat)
  mean(abs(y - yhat), na.rm = TRUE)
}

#' Normalized Mean Absolute Error (nMAE) Factory
#' 
#' Returns a function configured with a specific plant capacity for evaluation.
#' 
#' @param capacity A numeric value representing the max capacity of the plant.
#' @return A function that accepts two numeric vectors, \code{y} (targets) and 
#' \code{yhat} (predictions), and returns the nMAE normalized by the pre-configured 
#' capacity.
#' @examples
#' solar_nmae <- nmae(capacity = 500)
#' targets <- c(100, 200, 300)
#' predictions <- c(110, 190, 310)
#' solar_nmae(targets, predictions)
#' 
#' @export
nmae <- function(capacity) {
  if (capacity <= 0) stop("Capacity must be greater than zero.")
  
  function(y, yhat) {
    validate_metrics(y, yhat)
    mean(abs(y - yhat), na.rm = TRUE) / capacity
  }
}

#' Calculates the Weighted Mean Absoulte Error
#' 
#' @param y A numeric vector of target values.
#' @param yhat A numeric vector of predicted values.
#' @param weight A numeric vector of weight values.
#' @returns A numeric value 
#' @export
wmae <- function(y, yhat, weight) {
  sum(weight * abs(y - yhat)) / sum(weight)
}

#' Calculates the Mean Absolute Percentage Error (MAPE)
#' 
#' Note: Use with caution for solar data, as true zeros (nighttime) 
#' will produce infinite values. Consider using nMAE instead.
#' 
#' @param y A numeric vector of target values.
#' @param yhat A numeric vector of predicted values.
#' @returns A numeric value 
#' @export
mape <- function(y, yhat) {
  validate_metrics(y, yhat)
  
  if (any(y == 0, na.rm = TRUE)) {
    warning("Zero values detected in target vector 'y'. MAPE will return Inf. Consider filtering daylight hours or using nMAE.")
  }
  
  100 * mean(abs((y - yhat) / y), na.rm = TRUE)
}

#' Calculates the Relative Squared Error (RSE)
#' 
#' @param y A numeric vector of target values.
#' @param yhat A numeric vector of predicted values.
#' @returns A numeric value representing the ratio of model error to baseline error
#' @export
rse <- function(y, yhat) {
  validate_metrics(y, yhat)
  
  mean_y <- mean(y, na.rm = TRUE)
  
  # Calculate the sum of squared errors for the model and the baseline
  model_sse <- sum((y - yhat)^2, na.rm = TRUE)
  baseline_sse <- sum((y - mean_y)^2, na.rm = TRUE)
  
  if(baseline_sse == 0) {
    warning("Variance of target 'y' is zero (e.g., all night data). RSE will return NaN.")
    return(NaN)
  }
  
  model_sse / baseline_sse
}

