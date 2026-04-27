#' Stratified Train-Test Split for Solar Time-Series
#' 
#' @param df A dataframe containing solar data
#' @param tstamp_col Character vector; The string name of the timestamp column
#' @param period_in_minutes Integer; The interval resolution (e.g., 15 or 60)
#' @param prop Real number between 0 and 1; The proportion of data to include in the training set
#' @param prop_cal Real number between 0 and 1; the proportion of data to include in the calibration set
#' @return A list of data frames that contains `train` and `test` and `cal`
#' 
#' @export
train_test_split <- function(df, tstamp_col, period_in_minutes, prop = 0.8, prop_cal = 0) {

  stopifnot(any(c("POSIXct", "POSIXt") %in% class(df[[tstamp_col]])))
  if (prop < 0 | prop > 1 | prop_cal < 0 | prop_cal > 1) {
    stop("prop & prop_cal should be between 0 and 1")
  }

  prop_test <- 1 - prop

  total <- sum(c(prop, prop_test, prop_cal), na.rm = TRUE)

  #Recalibrate to get proportions
  prop_train <- prop / total
  prop_test <- prop_test / total
  prop_cal <- prop_cal / total

  # Prepare the dataframe with an ID and an interval group
  df_prepped <- df |>
    dplyr::mutate(
      interval = get_interval(df[[tstamp_col]], period_in_minutes),
      rid = dplyr::row_number()
    )

  #Stratified random sample
  train_df <- df_prepped |>
    dplyr::group_by(interval) |> 
    dplyr::slice_sample(prop = prop_train) |> 
    dplyr::ungroup()

  #Extract the remaining rows using the 'rid' key
  test_df <- df_prepped |>
    dplyr::anti_join(train_df, by = "rid")

  if (prop_cal > 0) {
    prop_cal <- prop_cal / (prop_test + prop_cal)
    cal_df <- test_df |> 
      dplyr::group_by(interval) |> 
      dplyr::slice_sample(prop = prop_cal) |> 
      dplyr::ungroup() 

    test_df <-  test_df |>
      dplyr::anti_join(cal_df, by = "rid")

    train_df <- train_df |> dplyr::select(-rid, -interval)
    test_df  <- test_df  |> dplyr::select(-rid, -interval)
    cal_df <- cal_df |> dplyr::select(-rid, -interval)

    return(
      list(
        train = train_df,
        test = test_df,
        cal = cal_df
      )
    )

  } else {
    train_df <- train_df |> dplyr::select(-rid, -interval)
    test_df  <- test_df  |> dplyr::select(-rid, -interval)

    return(
      list(
        train = train_df,
        test = test_df
      )
    )
  }

}

#' Sample training data based on physical weights
#' @param data The training dataframe containing a 'weight' column
#' @return A pruned dataframe
#' 
#' @export
prune_by_weight <- function(data) {
  # Generate a random number [0, 1] for every row
  # Keep the row if the weight is greater than the random number
  keep_idx <- stats::runif(nrow(data)) <= data$weight
  return(data[keep_idx, ])
}

`%||%` <- function(a, b) if (!is.null(a)) a else b


#' Load a saved SolarModel
#' @param path Path to the .rds file
#' @export
load_solar_model <- function(path) {
  if (!file.exists(path)) stop("File not found.")
  
  obj <- readRDS(path)
  
  # Safety check to ensure we loaded the right class
  if (!inherits(obj, "SolarModel")) {
    stop("The provided file does not contain a SolarModel object.")
  }
  
  return(obj)
}

#' Generate a report given a model and a test set.
#' 
#' @param test_df A data farme used for testing the model
#' @param model A instnace of class SolarModel
#' @param format A string; the fomrat of the report to be generated. Avialable values: "raw", "json", "data.frame". The default is "raw" i.e. a list is returned.
#' @param metric_list A list of named metric functions, from the available metrics
#' 
#' @return A report in a specified format
#' 
#' @export
generate_performance_report <- function(test_df, model, format = 'json', metric_list = list("rmse" = rmse, "nmae" = nmae)) {
  
  #Extract the column names for y (the target value)
  y_col <- all.vars(model$model_formula)[1]

  if (!(y_col %in% colnames(test_df))){
    stop("The column name of the target variable in the test dataset is expected to be: ", y_col)
  }

  #Extract the target values
  y <- test_df[[y_col]]
  #Predict on the dataset
  yhat <- model$predict(test_df)$yhat

  if ("nmae" %in% names(metric_list)) {
    metric_list$nmae <- nmae(model$capacity)
  }

  eval_results <- lapply(metric_list, FUN = function(f) f(y, yhat))

  names(eval_results) = names(metric_list)

  eval_results$y <- y
  eval_results$yhat <- yhat

  if (format == "data.frame") {
    eval_results |> as.data.frame()
  } else if (format == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      warning("jsonlite not installed, returning the default format")
      eval_results
    }
    jsonlite::toJSON(eval_results)
  } else {
    eval_results
  }

}