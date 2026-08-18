# ==============================================================================
# Fiber Photometry Signal Classification: Static Model & Inference Function
# ==============================================================================
library(dplyr)
library(tidyr)
library(glmnet)

# Feature definitions required by the model
fpmodelfeatures <- c(
  'sig_range_bins', 'sig_rms_bins', 'sig_MagSum_Band1', 'sig_MagSum_Band2', 'sig_MagSum_Band3',
  'baq_range_bins', 'baq_rms_bins', 'baq_MagSum_Band1', 'baq_MagSum_Band2', 'baq_MagSum_Band3',
  'baqscaled_range_bins', 'baqscaled_rms_bins', 'baqscaled_MagSum_Band1', 'baqscaled_MagSum_Band2', 'baqscaled_MagSum_Band3',
  'sigsub_range_bins', 'sigsub_rms_bins', 'sigsub_MagSum_Band1', 'sigsub_MagSum_Band2', 'sigsub_MagSum_Band3',
  'sigfilt_range_bins', 'sigfilt_rms_bins', 'sigfilt_MagSum_Band1', 'sigfilt_MagSum_Band2',
  'sigtobaq_rms_bins_scaled', 'sigtobaq_MagSum_Band1_scaled', 'sigtobaq_MagSum_Band2_scaled', 'sigtobaq_MagSum_Band3_scaled',
  'sigtobaqscaled_rms_bins_scaled', 'sigtobaqscaled_MagSum_Band1_scaled', 'sigtobaqscaled_MagSum_Band2_scaled', 'sigtobaqscaled_MagSum_Band3_scaled'
)

#' Save Trained Elastic Net Model and Metadata
saveFPmodel <- function(model, filepath = "fp_elastic_net_model.rds", 
                          feature_names = fpmodelfeatures, lambda_val = NULL) {
  if (inherits(model, "cv.glmnet")) {
    if (is.null(lambda_val)) lambda_val <- model$lambda.min
    base_model <- model$glmnet.fit
  } else {
    base_model <- model
    if (is.null(lambda_val)) lambda_val <- model$lambda[1]
  }
  
  coef_matrix <- as.matrix(coef(base_model, s = lambda_val))
  coef_df <- data.frame(
    feature = rownames(coef_matrix),
    weight  = coef_matrix[, 1],
    stringsAsFactors = FALSE
  )
  
  model_bundle <- list(
    model         = base_model,
    lambda        = lambda_val,
    features      = feature_names,
    coefficients  = coef_df,
    created_at    = Sys.time()
  )
  
  saveRDS(model_bundle, file = filepath)
  message(sprintf("Model successfully saved to: %s (lambda = %g)", filepath, lambda_val))
  invisible(model_bundle)
}

#' Preprocess Raw Photometry Data into Wide Model Format
preprocessFPdata <- function(data_raw, subjectvariables = NULL) {
  data <- data_raw
  if (!is.null(subjectvariables)) {
    data <- left_join(data, subjectvariables, by = "SubjectID")
  }
  if ("Sensor" %in% names(data)) {
    data <- data %>% filter(Sensor != 'GRABDA3M')
  }
  
  id_cols_present <- intersect(
    c("SubjectID", "Split", "Set", "Sex", "FiberPlacement", "Sensor", 
      "Rig", "Power", "Signal_RDScored", "Signal_RDQuality", "baqscalingfactor"),
    names(data)
  )
  
  if ("Variable" %in% names(data)) {
    data_wide <- data %>%
      pivot_wider(
        id_cols = all_of(id_cols_present),
        names_from = Variable,
        values_from = any_of(c("sig", "baq", "baqscaled", "sigsub", "sigfilt", "sigtobaq", "sigtobaqscaled"))
      )
  } else {
    data_wide <- data
  }
  
  ratio_cols <- c(
    "sigtobaq_mean_bins", "sigtobaq_range_bins", "sigtobaq_rms_bins",
    "sigtobaqscaled_mean_bins", "sigtobaqscaled_range_bins", "sigtobaqscaled_rms_bins",
    "sigtobaq_MagSum_Band1", "sigtobaq_MagSum_Band2", "sigtobaq_MagSum_Band3",
    "sigtobaqscaled_MagSum_Band1", "sigtobaqscaled_MagSum_Band2", "sigtobaqscaled_MagSum_Band3"
  )
  
  for (col in ratio_cols) {
    scaled_col_name <- paste0(col, "_scaled")
    if (col %in% names(data_wide) && !(scaled_col_name %in% names(data_wide))) {
      data_wide[[scaled_col_name]] <- data_wide[[col]] - 1
    }
  }
  
  return(data_wide)
}

#' Primary Inference Function (Customizable Threshold)
predictFPsignal <- function(new_data, model_obj, threshold = 0.5, return_full = TRUE) {
  # Load model bundle if filepath was passed
  if (is.character(model_obj)) {
    if (!file.exists(model_obj)) stop(sprintf("Model file not found: %s", model_obj))
    model_bundle <- readRDS(model_obj)
  } else if (is.list(model_obj) && "model" %in% names(model_obj)) {
    model_bundle <- model_obj
  } else if (inherits(model_obj, "glmnet") || inherits(model_obj, "cv.glmnet")) {
    model_bundle <- list(
      model    = if (inherits(model_obj, "cv.glmnet")) model_obj$glmnet.fit else model_obj,
      lambda   = if (inherits(model_obj, "cv.glmnet")) model_obj$lambda.min else model_obj$lambda[1],
      features = fpmodelfeatures
    )
  } else {
    stop("Invalid model_obj. Must be a .rds filepath, model bundle list, or glmnet object.")
  }
  
  required_features <- model_bundle$features
  missing_features <- setdiff(required_features, names(new_data))
  
  # Auto-preprocess if long format is passed
  if (length(missing_features) > 0) {
    if ("Variable" %in% names(new_data)) {
      new_data_wide <- preprocessFPdata(new_data)
      missing_features <- setdiff(required_features, names(new_data_wide))
      if (length(missing_features) > 0) {
        stop(paste("Missing required predictor columns:", paste(missing_features, collapse = ", ")))
      }
      data_for_pred <- new_data_wide
    } else {
      stop(paste("The input dataset is missing required features:", paste(missing_features, collapse = ", ")))
    }
  } else {
    data_for_pred <- new_data
  }
  
  # Format design matrix and predict
  x_matrix <- as.matrix(data_for_pred[, required_features, drop = FALSE])
  probs <- as.numeric(predict(model_bundle$model, newx = x_matrix, s = model_bundle$lambda, type = "response"))
  
  # Apply threshold
  predictions <- factor(ifelse(probs >= threshold, "Y", "N"), levels = c("N", "Y"))
  
  results <- data_for_pred
  results$Prob_Y <- round(probs, 2)
  results$Threshold_Used <- threshold
  results$Predicted <- predictions
  
  # Check accuracy if ground truth column is present
  actual_col <- intersect(c("Signal", "Signal_RDScored"), names(results))
  if (length(actual_col) > 0) {
    actual_vals <- factor(results[[actual_col[1]]], levels = c("N", "Y"))
    results$Correct <- (results$Predicted == actual_vals)
  }
  
  if (!return_full) {
    id_vars <- intersect(c("SubjectID", "Sensor", "FiberPlacement", "Rig", "Power", "Signal", "SignalQuality"), names(results))
    results <- results %>% select(all_of(id_vars), Prob_Y, Threshold_Used, Predicted, any_of("Correct"))
  }
  
  return(results)
}