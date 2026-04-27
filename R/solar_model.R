#' R6 Class for Solar Forecasting
#' 
#' @description
#' A high-level interface for training, predicting, and evaluating solar power models.
#' Encapsulates plant metadata, importance-weighting logic, and consistent metrics.
#' 
#' @field lat Numeric. Latitude of the territory or plant.
#' @field lon Numeric. Longitude of the territory or plant.
#' @field tz String. Timezone (e.g., "Europe/Athens" or "UTC").
#' @field capacity Numeric. Installed capacity used for cnMAE normalization.
#' @field period Integer. Time series frequency in minutes (default: 15).
#' @field beta Numeric. The exponential decay parameter for importance weights.
#' @field method String. "laplacian" or "gaussian".
#' @field model The trained model object (typically from e1071).
#' @field metrics List. A named list of metric functions.
#' @field tstamp_col String. The column name of the timestamp.
#' @field cal_df Data frame. A data frame used for conformal predictions.
#' @field q_hat Numeric. The quantile estimation used for calculating prediction intervals using conformal statistics. 
#' @field model_formula R formula. The formula passed to fit function to train the model.
#' @field scaling_params A list that contains the scale parameters if kernlab is used. As opposed to e1071 kernlab does nbot automatically handle the parameter scaling.
#' 
#' @importFrom R6 R6Class
#' @importFrom dplyr mutate filter .data
#' @importFrom stats predict
#' @export
SolarModel <- R6::R6Class(
  "SolarModel",
  
  public = list(
    lat = NULL,
    lon = NULL,
    tz = NULL,
    capacity = NULL,
    period = NULL,
    beta = NULL,
    method = NULL,
    model = NULL,
    metrics = NULL,
    tstamp_col = NULL,
    cal_df = NULL,
    q_hat = NULL,
    model_formula = NULL,
    scaling_params = NULL, # Stores mean/sd for kernlab

    #' @description Initialize a new SolarModel object.
    #' @param lat A numeric value representing latitude.
    #' @param lon A numeric value representing longitude.
    #' @param capacity A numeric value; the installed nameplate capacity.
    #' @param tz A string representing the timezone (default: "UTC").
    #' @param period An integer; the period of the time series in minutes (default: NULL).
    #' @param metrics An optional named list of metric functions.
    #' @param engine The SVM engine to use ("e1071" or "kernlab").
    initialize = function(lat, lon, capacity, period, tz = "UTC", engine = NULL, metrics = NULL) {
      if (is.null(lat) | is.null(lon) | is.null(capacity) | is.null(period)) {
        stop("Specify numeric values for lat, lon, capacity, period")
      } 

      self$lat <- lat
      self$lon <- lon
      self$capacity <- capacity
      self$tz <- tz
      self$period <- period
      private$..mode <- "train"
      
     
      self$metrics <- if (is.null(metrics)) {
        list(
          rmse  = rmse,
          mae   = mae,
          mape  = mape,
          nmae = nmae(capacity) 
        )
      } else {
        metrics
      }
    },

    
    #' @description Helper to prepare features and optionally split data.
    #' Automatically switches behavior based on the object's current mode. 
    #' The data returned depends on the current mode of the model. If the mode is train 
    #' train_test_split function is called.  
    #' 
    #' @param df Data frame to process.
    #' @param tstamp_col Character. The name of the timestamp column in \code{df}.
    #' @param beta Numeric. The decay parameter for importance weights (default: 0.01).
    #' @param method Character. The weighting method, either "laplacian" or "gaussian".
    #' @param prop Numeric. The proportion of data for the training set (default: 0.8).
    #' @param prop_cal Numeric. The proportion of data for the calibration set (default: 0).
    #' @param detect_outliers Logical. If set to TRUE a one class SVM will be trained, will detect outliers and place a flag at each row in the training set.
    #' @param drop_outliers Logical. If set to TRUE the outliers will be removed from the training set.
    #' @param add_weights Logial. Add weights to data frame rows.
    #' @param model_formula An object of class \code{formula} describing the model.
    #' @param ... Additional arguments: \code{perc_conf} (percentage confidence) if a one class svm is used for detecting outliers 
    #' where \code{perc_conf} expresses the level of confidence (percentage) with which we would like the model to label a data point as being an outlier. 
    #' and \code{col_names}, a character vector of column names from the training data frame to be passed in the one-class SVM.
    #' 
    #' @return In "train" mode, a list with \code{train_df} and \code{test_df}. In "eval" mode, a processed data frame.
    prepare_data = function(df, tstamp_col,  model_formula, prop = 0.8, prop_cal = 0, method = "laplacian",  beta = 0.01, add_weights = FALSE, detect_outliers = TRUE, drop_outliers = FALSE, ...) {

      #Set the timestamp column
      self$tstamp_col <- tstamp_col
      
      if (!any(c("POSIXct", "POSIXt") %in% class(df[[tstamp_col]]))) {
        stop("The timestamp column must be of class POSIXct or POSIXt.", call. = FALSE)
      }
      
      self$beta <- beta
      self$method <- method

      df_processed <- df |>
        add_cyclic_components(time_col = tstamp_col) |>
        dplyr::mutate(
          is_daylight = is_daylight(.data[[tstamp_col]], lat = self$lat, lon = self$lon, tz = self$tz),
          interval = get_interval(.data[[tstamp_col]], self$period)
        )
      
      df_processed <- if (add_weights) {
        df_processed |>
          add_weights(
            tstamp_col = tstamp_col,
            beta       = self$beta,
            period     = self$period,
            method     = self$method,
            lat        = self$lat,
            lon        = self$lon,
            tz         = self$tz
          )
      } else {
        df_processed
      }
      
      if (private$..mode == "train") {
        #Filter out the data where production is zero due to light conditions
        df_train_ready <- df_processed |> dplyr::filter(.data$is_daylight)
        df_list <- train_test_split(df_train_ready, tstamp_col, self$period, prop, prop_cal)

        #Expand the formula to include the sine and cosine components
        model_formula <- update.formula(model_formula, . ~ . + qsin + qcos)
        self$model_formula <- model_formula

        
        if (prop_cal > 0) {
          #Save the cal_df. Then during the fit method it will be possible to predict on this dataset and 
          #call calibrate to determine the prediction intervals
          self$cal_df <- df_list$cal
        }
        if (detect_outliers) {
          args <- list(...)
          col_names <- args$col_names
          if (!is.null(col_names)) {
            perc_conf <- args$perc_conf
            #Select the column names to pass into the ove-class SVM
            dfo <- df_list$train |> dplyr::select(dplyr::all_of(col_names))

            is_outlier <- train_oc_model(dfo, perc_conf)
            df_list$train$is_outlier <- is_outlier
            #Put the outliers in a separate data frame and add it to the list
            df_list$outliers <- df_list$train |>
              dplyr::filter(is_outlier) |>
              dplyr::select(-is_outlier)
            
            #Remove the outliers from the trainign set
            df_list$train <- df_list$train |>
              dplyr::filter(!is_outlier) |>
              dplyr::select(-is_outlier)

            if (drop_outliers) {
              df_list$outliers <- NULL
            }
          }
        }

        return(df_list)
      } else {

        return(df_processed)
      }
    },

    #' @description Fit the model using the internal training engine. 
    #' Automatically chooses between standard training and beta co-optimization.
    #' 
    #' @param train_df Data frame containing the training data.
    #' @param test_df Data frame containing the validation/test data.
    #' @param parallel A logical value indicating whether a custom parallel tuning should be used
    #' @param ... Optional arguments:
    #' \itemize{
    #'   \item \code{n_folds}: N integer, the number of folds to use in parallel tuning
    #'   \item \code{ctrl}: A list of control parameters for the tuning process.
    #'   \item \code{train_grid}: A data frame or list defining the SVM hyperparameter 
    #'         search space.
    #' }
    #'   
    #' @return A list containing \code{metrics} (test set performance) and 
    #' \code{yhat} (predictions on the test dataset).
    fit = function(train_df, test_df, parallel = FALSE, ...) {

      if (is.null(self$tstamp_col)) {
        stop("Call prepare_data before fitting the model")
      }

      private$..mode <- "train"
      args <- list(...)

      train_results <- if (parallel) {
        message("Parallel tuning started")
        args <- list(...)
        
        train_grid <- if (is.null(args$train_grid)) {
          message("No train_grid detected, setting default")
          list(cost = 2^(-2:5), gamma = 2^seq(-2, 1, by = 0.5))
        } else {
          args$train_grid
        }

        train_grid <- expand.grid(
          cost = train_grid$cost,
          gamma = train_grid$gamma,
          epsilon = c(0.01, 0.1)
        )

        parallel_tune_svm(
          train_df, 
          test_df, 
          self$model_formula, 
          self$metrics, 
          train_grid,
          n_folds = args$n_folds %||% 5
        )
        
      } else {
        message("Sequential tuning started")
        train_model(
          train_df       = train_df,
          test_df        = test_df,
          model_formula  = self$model_formula,
          metrics_list   = self$metrics,
          ctrl           = args$ctrl,
          train_grid     = args$train_grid
        )
      }
      
      self$model <- train_results$best_model
      
      message("Model trained successfully. Switching to 'eval' mode.")
      private$..mode <- "eval"

      #If a calibration dataset has been defined use it to calculate prediction intervals using
      #quantiles and scale them by weights.
      #calculating all the residuals and then the scores which are basically some percentage of the capacity.
      # Then i sort those from the lowest to the highest value.
      # Then instead of taking the 90% percentile directly, i sort the weights with the same order 
      # as the scores. So, basically i am saying: "look this score in the sorted score list is 
      # low but its corresponding weight is low or it may be high". 
      # If it is low basically ignore it and move further done the list until 
      # we have accounted for 90% of the total weight mass.
      if (!is.null(self$cal_df)) {
        message("Calibrating adaptive conformal intervals...")

        self$cal_df <- self$cal_df |>
          add_weights(
            tstamp_col = self$tstamp_col,
            beta       = self$beta,
            period     = self$period,
            method     = self$method,
            lat        = self$lat,
            lon        = self$lon,
            tz         = self$tz
          )
        
        #Generate predictions on the calibration set
        yhat_cal <- as.numeric(stats::predict(self$model, newdata = self$cal_df))
        yhat_cal <- pmax(0, yhat_cal) 
        
        target_var <- all.vars(self$model_formula)[1]
        y_cal <- as.numeric(self$cal_df[[target_var]])
        
        #Define the Scaling Factor (Heuristic Uncertainty) and use 5% (0.05) as the 'noise floor' to prevent division-by-zero
        cap <- self$capacity 
        beta_floor <- 0.05 
        sigma_cal <- (yhat_cal / cap) + beta_floor
        
        # Calculate Normalized Non-conformity Scores. These are basically some portion of the 
        # capacity.
        s_scores <- abs(y_cal - yhat_cal) / sigma_cal
        
        # Weighted Quantile Calculation
        # Uses weights 
        alpha <- 0.1
        
        #Sort the scores
        ord <- order(s_scores)
        s_sorted <- s_scores[ord]
        #Sort the weights
        w_sorted <- self$cal_df$weight[ord] 
        
        #The q_hat is one of those scores. Which one? We use the 
        #quantile on the sorted weights. 
        cum_w <- cumsum(w_sorted) / sum(w_sorted)
        self$q_hat <- s_sorted[which(cum_w >= (1 - alpha))[1]]
        
        message("Conformal Multiplier (q_hat) calibrated: ", round(self$q_hat, 2))
      }

      # Return the test set performance and predictions
      return_list <- if (parallel) {
        list(metrics = train_results$test_metrics, 
             yhat = train_results$yhat,
             trials = train_results$trials)
      } else {
        list(metrics = train_results$test_metrics, 
             yhat = train_results$yhat)
      }

      return(return_list)
    },

    #' @description Generate physical-floor-clipped predictions.
    #' @param new_data Data frame containing features for prediction.
    #' @param tstamp_col Character. The name of the timestamp column in \code{new_data}.
    #' @return A data frame with columns \code{date_time} and \code{yhat}.
    #' 
    #' @param newdata Data frame for which to generate predictions.
    predict = function(new_data) {
      if (is.null(self$model)) stop("Model must be trained before predicting.", call. = FALSE)
      
      # Force eval mode for prediction data prep
      private$..mode <- "eval"
      
      if (!is.null(self$q_hat)) {

        prepared_df <- self$prepare_data(new_data, 
                                       self$tstamp_col, 
                                       beta   = self$beta, 
                                       method = self$method,
                                       add_weights = TRUE)
        
        y_pred <- stats::predict(self$model, prepared_df)
        cap <- self$capacity
        beta_floor <- 0.05
        
        sigma_new <- (y_pred / cap) + beta_floor
        margin <- self$q_hat * sigma_new

        rdf <- data.frame(
          date_time = prepared_df[[self$tstamp_col]],
          yhat      = pmax(0, as.numeric(y_pred))) |>
          dplyr::mutate(
            lower = pmax(0, yhat - margin),
            upper = yhat + margin)
        
        return(rdf)

      } else {
        prepared_df <- self$prepare_data(new_data, 
                                       self$tstamp_col, 
                                       beta   = self$beta, 
                                       method = self$method)
        
        y_pred <- stats::predict(self$model, prepared_df)
        rdf <- data.frame(
          date_time = prepared_df[[self$tstamp_col]],
          yhat      = pmax(0, as.numeric(y_pred))
        )

        colnames(rdf) <- c(self$tstamp_col, "yhat")

        return(rdf)
      }
    },

    #' @description Save the current model instance to an RDS file
    #' @param path Character string. The path where the model should be saved.
    save_model = function(path) {
      if (is.null(path)) stop("A valid file path must be provided.")
      
      message("Saving SolarModel instance to: ", path)
      saveRDS(self, file = path)
    }

  ),

  active = list(
    #' @field mode Access or set the current operational mode ("train" or "eval").
    mode = function(value) {
      if (missing(value)) return(private$..mode)
      
      if (!(value %in% c("train", "eval"))) stop("Mode must be 'train' or 'eval'")
      private$..mode <- value
    }
  ),
  
  private = list(
    ..mode = NULL 
  )
)