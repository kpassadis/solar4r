#' Train, tune, and evaluate an SVM model using the e1071 package
#' 
#' @param train_df Data frame used to train the model.
#' @param test_df Data frame used to evaluate the model (must have same features).
#' @param model_formula Formula of type `target ~ covariates`.
#' @param metrics_list A named list of functions used for evaluation (e.g., list(rmse = rmse, mae = mae)).
#' @param ctrl SVM tune control.
#' @param train_grid SVM hyperparameter grid.
#' @returns A list with the model, hyperparameters, and test metrics.
#' @importFrom stats predict
#' @export
train_model <- function(
    train_df, 
    test_df, 
    model_formula, 
    metrics_list, 
    ctrl = NULL,
    train_grid = NULL
) {

  ctrl <- if (is.null(ctrl)) {
    e1071::tune.control(sampling='cross', cross=5, nrepeat=1)
  } else {
    ctrl
  }

  train_grid <- if (is.null(train_grid)) {
    list(cost = 2^(-2:5), gamma = 2^seq(-2, 1, by = 0.5))
  } else {
    train_grid
  }

  #Tune the model using Cross-Validation on the training set
  tuned <- e1071::tune(
    e1071::svm, 
    model_formula, 
    data = train_df, 
    kernel = 'radial', 
    ranges = train_grid, 
    tunecontrol = ctrl,
    type = "eps-regression"
  )
  
  best_model <- tuned$best.model

  #Extract the target variable name dynamically from the formula
  target_col <- all.vars(model_formula)[1]
  
  #Generate predictions on the Unseen Test Set
  y_true <- test_df[[target_col]]
  y_pred <- stats::predict(best_model, newdata = test_df)
  y_pred <- pmax(0, as.numeric(y_pred))

  #yhat_df <- data.frame(
  #  tstamp = test_df[[tstamp_col]], # This keeps the link alive!
  #  yhat   = pmax(0, as.numeric(y_pred))
  #)

  # Evaluate all passed metrics dynamically by applying the metric funcs
  eval_results <- lapply(metrics_list, function(metric_fn) {
    metric_fn(y_true, y_pred)
  })

  list(
    cv_rmse      = tuned$best.performance, 
    test_metrics = eval_results,           
    best_model   = best_model,
    gamma        = best_model$gamma,
    cost         = best_model$cost,
    yhat         = y_pred
  )
}


#' Parallel Tuning for SVM
#' 
#' @description Performs a parallelized grid search to find optimal SVM 
#' hyperparameters (cost, gamma, epsilon) using MSE as the internal metric.
#' 
#' @param train_df Data frame containing training data.
#' @param test_df Data frame containing unseen test data for final evaluation.
#' @param model_formula The formula to fit (e.g., pv ~ irrad + temp).
#' @param metrics_list A list of functions to evaluate test performance.
#' @param train_grid A data frame of parameters to test.
#' @param n_folds Number of cross-validation folds. Defaults to 5.
#' 
#' @return A list containing the best model, test metrics, and predictions.
#' @export
parallel_tune_svm <- function(train_df, test_df, model_formula, metrics_list, train_grid, n_folds = 5) {
  
  # A note whgenj executing parallel code: 
  # Running this function from the R6 class yields: 
  # Error in `checkForRemoteErrors()`:! 2 nodes produced errors; first error: is.numeric(lat) is not TRUE
  # even if lat is numeric. Why does this happen then? 
  # The reason is the lazy nature of R. In R arguments are in fact "promises" and are never evaluated
  # until they are used. When shipping to cluster it maybe the case that instead of an actual numeric
  # value a primise is being shipped, and this cause the error.
  # The solution is to force the evaluation before making the cluster. Also to be 100% safe that there
  # are no other conflicts wrap everything in a list called constants. 
  # Force evaluation to break the R6 'promise'
  force(n_folds)

  #Setup Parallel Cluster
  n_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl))

  # Export necessary data and libraries to workers
  parallel::clusterExport(cl, varlist = c("train_df", "model_formula", "n_folds", "train_grid"), envir = environment())
  parallel::clusterEvalQ(cl, {
    library(e1071)
  })


  # Create CV Folds (Indices). Set the seed for reproducability.
  set.seed(123)
  folds <- sample(cut(seq_len(nrow(train_df)), breaks = n_folds, labels = FALSE))

  message("starting parallel training of ", nrow(train_grid), " models")

  # We iterate over every row in the train_grid
  results <- parallel::parLapply(cl, seq_len(nrow(train_grid)), function(i) {
    params <- train_grid[i, ]
    fold_errors <- numeric(n_folds)
    
    for (f in 1:n_folds) {
      # Split
      cv_train <- train_df[folds != f, ]
      cv_val   <- train_df[folds == f, ]
      
      # Fit (including weights if present in train_df)
      m <- e1071::svm(
        model_formula, 
        data = cv_train,
        cost = params$cost,
        gamma = params$gamma,
        epsilon = params$epsilon
      )
      
      preds <- stats::predict(m, cv_val)
      target_col <- all.vars(model_formula)[1]
      fold_errors[f] <- sqrt(mean((cv_val[[target_col]] - preds)^2))
    }
    
    return(mean(fold_errors))
  })

  #Winner
  train_grid$rmse <- unlist(results)
  best_params <- train_grid[which.min(train_grid$rmse), ]
  
  
  message("Best Params: Cost=", best_params$cost, ", Gamma=", best_params$gamma)
  
  #Train a model with the optimal specification and the full training dataset
  best_model <- e1071::svm(
    model_formula,
    data = train_df,
    cost = best_params$cost,
    gamma = best_params$gamma,
    epsilon = best_params$epsilon
  )
  
  target_col <- all.vars(model_formula)[1]
  
  #Generate predictions on the Unseen Test Set
  y_true <- test_df[[target_col]]
  y_pred <- stats::predict(best_model, newdata = test_df)
  y_pred <- pmax(0, as.numeric(y_pred))

  # Evaluate all passed metrics dynamically by applying the metric funcs
  eval_results <- lapply(metrics_list, function(metric_fn) {
    metric_fn(y_true, y_pred)
  })

  list(
    cv_rmse      = best_params$cost, 
    test_metrics = eval_results,           
    best_model   = best_model,
    gamma        = best_model$gamma,
    cost         = best_model$cost,
    yhat         = y_pred,
    trials       = train_grid
  )
}

#' Predict solar production with automatic night-time zeroing
#' 
#' @param model A trained SVM model.
#' @param beta The optimized beta value used during training.
#' @param method A character vector "laplacian" or "gaussian"
#' @param period An integer period in minutes
#' @param newdata A data frame that contains unseen data.
#' @param tstamp_col The string name of the timestamp column.
#' @param lat A numeric value representing latitude.
#' @param lon A numeric value representing longitude.
#' @param tz String representing the timezone (default "UTC").
#' 
#' @importFrom stats predict
#' @importFrom dplyr mutate filter bind_rows select arrange across all_of .data
#' @export
ppredict <- function(model, beta, method, newdata, tstamp_col, period, lat, lon, tz = "UTC") {

  newdata_prepped <- newdata |>
    dplyr::mutate(
      is_daylight = is_daylight(.data[[tstamp_col]], lat, lon, tz)
    ) |>
    add_weights(tstamp_col = tstamp_col, 
                beta = beta, 
                period = period, 
                method = method, 
                lat = lat, 
                lon = lon)

  #Split into Day and Night
  df_day <- newdata_prepped |> dplyr::filter(.data$is_daylight)
  df_night <- newdata_prepped |> dplyr::filter(!.data$is_daylight)

  #Predict for Daylight
  if (nrow(df_day) > 0) {
    df_day$yhat <- stats::predict(model, df_day)
    df_day$yhat <- pmax(0, df_day$yhat) 
  }

  #Zero out for Night
  if (nrow(df_night) > 0) {
    df_night$yhat <- 0
  }

  # Combine and Sort using across(all_of()) to avoid 'no visible binding' complains when building
  combined <- dplyr::bind_rows(df_day, df_night) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(tstamp_col)))

  # Return only the essential columns
  combined |>
    dplyr::select(dplyr::all_of(tstamp_col), .data$yhat)
}

#' Detect outliers in the training set
#' The critical parameter in this function is perc_conf represents the confidence with which a pattern in the input data frame
#' is charactetized as being an outlier. Specifying a high confidence level is an expression of preference to true positives with the trade-off that some
#' outliers will remain undetected.
#' 
#' @param data_frame The data frame that contains the training data
#' @param perc_conf An integer close to (but less than) 100 
#' 
#' @examples
#' col_names <- c("pv", "irrad", "temp", "clds")
#' train_oc_model(solar[, col_names])
#' 
#' @returns A vector of logical values 
#' 
#' @export
train_oc_model <- function(data_frame, perc_conf = NULL) {

  perc_conf <- if (is.null(perc_conf)) 99 else perc_conf

  perc_conf <- if (perc_conf > 100 | perc_conf < 0) {
    99
  } else {
    perc_conf
  }

  nu <- (100 - perc_conf) / 100

  one_class_model <- e1071::svm(
    x = data_frame,
    type = "one-classification",
    kernel = "radial",
    nu = nu,   
    gamma = 0.1 
  )

  !stats::predict(one_class_model, data_frame)

}