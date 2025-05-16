#####function necessary to create scenarios#####

scenario <- function(sams, quadratic, missm, quadpred, missp, cov, test = FALSE){
  
  # this function takes each factor to be varied in simulation as input
  # and generates the mean and covariance matrices
  
  # sams      : sample size
  # quadratic : quadratic relationship between predictors and outcome variable
  # missm     : missing data mechanism wherre 0 = MCAR, 1 = weak-MAR, 2 = strong-MAR, 3 = weak-MNAR 4 = strong-MNAR
  # quadpred  : quadratic relationship between X1 and every other predictor (X2 : X8)
  # missp     : missing data percentage
  # cov       : strength of covariance between predictors
  # test      : test data frame (by default FALSE)
  
  
  
  # vector with means of predictors
  mu    <-  c(0,0,0,0,0,0,0)                 
  
  # covariance matrix with 1 on the diagonal and 0.4 off-diagonal (like in the Sisk paper but with only continous variables)
  cov_matrix <- matrix(cov, nrow = 7, ncol = 7)
  diag(cov_matrix) <- 1
  
  return(list("sams"       = sams, 
              "quadratic"  = quadratic,
              "missm"      = missm,
              "mu"         = mu, 
              "sigma"      = cov_matrix,
              "missp"      = missp,
              "quadpred"   = quadpred,
              "cov"        = cov,
              "test"       = test))
  
}




##### Data Generating function #####

generate_data <- function(inn) {
  
  # extract parameters from the input list
  sams      <- inn[[1]]      # sample size (number)
  quadratic <- inn[[2]]      # quadratic relationship flag
  missm     <- inn[[3]]      # missing data mechanism (0, 1, 2, 3, 4)
  mu        <- inn[[4]]      # mean vector
  sigma     <- inn[[5]]      # covariance matrix
  missp     <- inn[[6]]      # missing data percentage (0-1)
  quadpred  <- inn[[7]]      # quadratic relationship between predictors flag
  cov       <- inn[[8]]      # strength of covariance between predictors
  test_flag <- inn[[9]]      # test flag
  
  # Adjust sample size for test data 
  sams_test <- ifelse(test_flag, sams * 100, sams)
  
  # Generate Predictors 
  X <- mvrnorm(sams, mu, sigma)  # for training data
  X_test <- mvrnorm(sams_test, mu, sigma)  # for test data
  
  # Add an empty column for X1
  X <- cbind(rep(0, nrow(X)), X)
  X_test <- cbind(rep(0, nrow(X_test)), X_test)
  
  #Generate Relationship for X1 
  # (Linear or quadratic, low/high covariance as in your code)
  if (!quadpred && cov == 0.2) {
    a <- 1/11
    sigma_e <- sqrt(0.6706)
    X[,1] <- a * (X[,2] + X[,3] + X[,4] + X[,5] + X[,6] + X[,7] + X[,8]) +
      rnorm(sams, 0, sigma_e)
    X_test[,1] <- a * (X_test[,2] + X_test[,3] + X_test[,4] + X_test[,5] +
                         X_test[,6] + X_test[,7] + X_test[,8]) +
      rnorm(sams_test, 0, sigma_e)
  }
  if (!quadpred && cov == 0.8) {
    a <- (0.8)/(5.8)
    sigma_e <- sqrt(0.228)
    X[,1] <- a * (X[,2] + X[,3] + X[,4] + X[,5] + X[,6] + X[,7] + X[,8]) +
      rnorm(sams, 0, sigma_e)
    X_test[,1] <- a * (X_test[,2] + X_test[,3] + X_test[,4] + X_test[,5] +
                         X_test[,6] + X_test[,7] + X_test[,8]) +
      rnorm(sams_test, 0, sigma_e)
  }
  if (quadpred && cov == 0.2) {
    a <- 1/11
    sigma_e <- sqrt(0.6706)
    X[,1] <- a * (X[,2]^2 + X[,3]^2 + X[,4]^2 + X[,5]^2 + X[,6]^2 + X[,7]^2 + X[,8]^2) +
      rnorm(sams, 0, sigma_e)
    X_test[,1] <- a * (X_test[,2]^2 + X_test[,3]^2 + X_test[,4]^2 + X_test[,5]^2 +
                         X_test[,6]^2 + X_test[,7]^2 + X_test[,8]^2) +
      rnorm(sams_test, 0, sigma_e)
  }
  if (quadpred && cov == 0.8) {
    a <- (0.8)/(5.8)
    sigma_e <- sqrt(0.228)
    X[,1] <- a * (X[,2]^2 + X[,3]^2 + X[,4]^2 + X[,5]^2 + X[,6]^2 + X[,7]^2 + X[,8]^2) +
      rnorm(sams, 0, sigma_e)
    X_test[,1] <- a * (X_test[,2]^2 + X_test[,3]^2 + X_test[,4]^2 + X_test[,5]^2 +
                         X_test[,6]^2 + X_test[,7]^2 + X_test[,8]^2) +
      rnorm(sams_test, 0, sigma_e)
  }
  
  # Generate Outcome Variable 
  if (!quadratic) {
    Y <- 1.5 * X[,1] + 1.5 * X[,2] + 0.5 * X[,3] + 0.5 * X[,4] + rnorm(sams, 0, 1)
    Y_test <- 1.5 * X_test[,1] + 1.5 * X_test[,2] + 0.5 * X_test[,3] + 0.5 * X_test[,4] + rnorm(sams_test, 0, 1)
  } else {
    Y <- 1.5 * X[,1]^2 + 1.5 * X[,2]^2 + 0.5 * X[,3]^2 + 0.5 * X[,4]^2 + rnorm(sams, 0, 1)
    Y_test <- 1.5 * X_test[,1]^2 + 1.5 * X_test[,2]^2 + 0.5 * X_test[,3]^2 + 0.5 * X_test[,4]^2 + rnorm(sams_test, 0, 1)
  }
  
  # Introduce Missingness for Training Data 
  if (!test_flag) {
    if (missm == 0) {
      M <- rbinom(sams, 1, missp)
    } else if (missm == 1) {
      MARrank <- rank(X[,2])
      mar_prob_rank <- MARrank / sams
      mar_prob_random <- runif(sams)
      weight <- 0.5
      mar_prob <- weight * mar_prob_rank + (1 - weight) * mar_prob_random
      M <- rbinom(sams, 1, mar_prob * (missp/mean(mar_prob)))
    } else if (missm == 2) {
      MARrank <- rank(X[,2])
      mar_prob <- MARrank / sams
      M <- rbinom(sams, 1, mar_prob * (missp/mean(mar_prob)))
    } else if (missm == 3) {
      MNARrank <- rank(X[,1])
      mnar_prob_rank <- MNARrank / sams
      mnar_prob_random <- runif(sams)
      weight <- 0.5
      mnar_prob <- weight * mnar_prob_rank + (1 - weight) * mnar_prob_random
      mnar_prob <- mnar_prob * (missp / mean(mnar_prob))
      M <- rbinom(sams, 1, mnar_prob)
    } else if (missm == 4) {
      MNARrank <- rank(X[,1])
      mnar_prob <- MNARrank / sams
      M <- rbinom(sams, 1, mnar_prob * (missp/mean(mnar_prob)))
    }
    
    # Save true X1 before missingness
    X1_true <- X[,1]
    # Impose missingness in X1
    X[,1] <- ifelse(M == 1, NA, X[,1])
    
    # Format training data frame
    train_df <- cbind(X[,1:8], Y, X1_true, M) %>% 
      as.data.frame() %>% 
      setNames(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "Y", "X1_true", "M"))
  } else {
    train_df <- NULL  # not used when test_flag is TRUE
  }
  
  # Format test data frame (no missingness)
  test_df <- cbind(X_test[,1:8], Y_test) %>% 
    as.data.frame() %>% 
    setNames(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "Y"))
  
  return(list(train = train_df, test = test_df))
}


#####Function to apply model combinations#####
apply_models <- function(df, test_df, imp, subs) {
  
  # Imputation model:
  if (imp == 0) {
    imputed_data <- mice(df[1:9], method = "pmm", m = 1, maxit = 5, pmm.k = 1)
    complete_df <- complete(imputed_data, action = 1)
    
  } else if (imp == 1) {
    imputed_data <- mice(df[1:9], method = "norm.predict", m = 1, maxit = 5)
    complete_df <- complete(imputed_data, action = 1)
    
  } else if (imp == 2) {
    imputed_data <- mice(df[1:9], method = "rf", m = 1, maxit = 5)
    complete_df <- complete(imputed_data, action = 1)
    
  } else if (imp == 3) {
    df <- df %>% mutate(across(2:9, ~ .^2, .names = "{.col}_sq"))
    imputed_data <- mice(df[, c(1:9, 12:19)], method = "norm.predict", m = 1, maxit = 5)
    complete_df <- complete(imputed_data, action = 1)
    
  } else if (imp == 4) {
    imputed_data <- mice(df[1:9], method = "quadratic", m = 1, maxit = 5, quad.outcome = "Y", pmm.k = 1)
    complete_df <- complete(imputed_data, action = 1)
  } else {
    stop("Invalid value for imp. Must be 0, 1, 2, 3, or 4")
  }
  
  # Substantive model 
  if (subs == 0) {
    trained_lm <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 +
                       I(X1^2) + I(X2^2) + I(X3^2) + I(X4^2) +
                       I(X5^2) + I(X6^2) + I(X7^2) + I(X8^2), 
                     data = complete_df)
    predictions <- predict(trained_lm, newdata = test_df)
    MSE <- mean((test_df$Y - predictions)^2)
    calibration_model <- lm(test_df$Y ~ predictions)
    calibration_intercept <- coef(calibration_model)[1]
    calibration_slope <- coef(calibration_model)[2]
    R2 <- summary(calibration_model)$r.squared
  } else if (subs == 1) {
    best_cv_mse <- Inf
    best_model <- NULL
    best_params <- NULL
    ntree_vals <- c(500, 1000, 1500)
    nodesize_vals <- c(2, 5, 8)
    mtry_vals <- c(3, 5, 7)
    trControl <- trainControl(method = "cv", number = 5)
    
    for (nt in ntree_vals) {
      for (ns in nodesize_vals) {
        tunegrid <- expand.grid(mtry = mtry_vals)
        rf_caret <- train(
          Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8,
          data = complete_df,
          method = "rf",
          trControl = trControl,
          tuneGrid = tunegrid,
          ntree = nt,
          nodesize = ns
        )
        cv_mse <- min(rf_caret$results$RMSE^2)
        if (cv_mse < best_cv_mse) {
          best_cv_mse <- cv_mse
          best_params <- c(ntree = nt, nodesize = ns, mtry = rf_caret$bestTune$mtry)
          best_model <- rf_caret$finalModel
        }
      }
    }
    
    predictions <- predict(best_model, newdata = test_df)
    MSE <- mean((test_df$Y - predictions)^2)
    calibration_model <- lm(test_df$Y ~ predictions)
    calibration_intercept <- coef(calibration_model)[1]
    calibration_slope <- coef(calibration_model)[2]
    R2 <- summary(calibration_model)$r.squared
  } else {
    stop("Invalid value for subs. Must be 0 or 1.")
  }
  
  # Store predictions in test_df for plotting
  test_df$predictions <- predictions
  
  # Fit a LOESS model to smooth the relationship between predictions and observed Y.
  loess_fit <- loess(Y ~ predictions, data = test_df, span = 0.75)
  
  # Create a grid of prediction values over the range of test_df predictions
  pred_grid <- data.frame(predictions = seq(min(test_df$predictions), 
                                            max(test_df$predictions), 
                                            length.out = 200))
  
  # Get the LOESS-smoothed estimates for the grid
  smoothed_values <- predict(loess_fit, newdata = pred_grid)
  
  # Save the 200 LOESS observations in a data frame:
  calibration_data <- data.frame(
    predicted = pred_grid$predictions,
    loess_smoothed = smoothed_values
  )
  
  # Create a summary results data frame
  results_df <- data.frame(
    R2 = R2,
    MSE = MSE,
    Calibration_Intercept = calibration_intercept,
    Calibration_Slope = calibration_slope
  )
  
  return(list(
    Results_DataFrame = results_df,
    Calibration_Data = calibration_data
  ))
}




###### Define Simulation Parameters ######
sams      <- rep(1000, 40)
quadratic <- c(rep(TRUE, 20), rep(FALSE, 20))
quadpred  <- c(rep(TRUE, 10), rep(FALSE, 10), rep(TRUE, 10), rep(FALSE, 10))
cov       <- rep(c(rep(0.2, 5), rep(0.8, 5)), 4)
missm     <- rep(c(0, 1, 2, 3, 4), 8)
missp     <- rep(0.3, 40)

# Create a data frame with the 40 scenarios (only the six needed parameters)
sim_params <- data.frame(
  sams = sams,
  quadratic = quadratic,
  quadpred = quadpred,
  cov = cov,
  missm = missm,
  missp = missp
)


# Execute function
run_iteration <- function(iter) {
  # Number of scenarios for each iteration
  num_scenarios <- 40
  
  # Create an overall results folder (if not already present)
  results <- "results"
  if (!dir.exists(results)) dir.create(results)
  
  # Create folder structure for this iteration inside the overall results folder
  iter_folder <- file.path(results, paste0("ir_iteration_", iter))
  results_summary_folder <- file.path(iter_folder, "results_summary")
  calibration_folder <- file.path(iter_folder, "calibration_data")
  
  if (!dir.exists(iter_folder)) dir.create(iter_folder)
  if (!dir.exists(results_summary_folder)) dir.create(results_summary_folder)
  if (!dir.exists(calibration_folder)) dir.create(calibration_folder)
  
  # Create a cluster (adjust number of cores as needed)
  cl <- parallel::makeCluster(5)
  doParallel::registerDoParallel(cl)  # Register the cluster as the parallel backend
  
  # Use the iteration number as the seed for reproducibility with doRNG
  doRNG::registerDoRNG(iter)
  
  # Load required packages on each worker, and set custom library path
  parallel::clusterEvalQ(cl, {
    # Prepend the custom library folder so it's searched first
    .libPaths(c("~/Rlibs", .libPaths()))
    library(dplyr)
    library(mice)
    library(randomForest)
    library(caret)
    library(MASS)
  })
  
  # Export necessary objects and functions to the workers.
  parallel::clusterExport(cl, 
                          varlist = c("apply_models", "scenario", "generate_data", "sim_params"), 
                          envir = environment())
  
  # Define grid of model combinations: imp (0 to 4) and subs (0 to 1)
  model_combinations <- expand.grid(imp = 0:4, subs = 0:1)
  
  # Parallel loop: Process each of the 40 scenarios for this iteration.
  results_list_iter <- foreach::foreach(i = 1:num_scenarios,
                                        .packages = c("dplyr", "mice", "randomForest", "caret"),
                                        .combine = 'c') %dorng% {
                                          # Get the scenario parameters for scenario i (each row in sim_params)
                                          sp <- sim_params[i, , drop = FALSE]
                                          param_list <- list(
                                            sams      = sp$sams,
                                            quadratic = sp$quadratic,
                                            missm     = sp$missm,
                                            quadpred  = sp$quadpred,
                                            missp     = sp$missp,
                                            cov       = sp$cov,
                                            test      = FALSE
                                          )
                                          scenario_config <- do.call(scenario, param_list)
                                          
                                          # Generate data (returns a list with elements train and test)
                                          data_list <- generate_data(scenario_config)
                                          train_df <- data_list$train
                                          test_df  <- data_list$test
                                          
                                          # List to hold summary results for each model combination in this scenario
                                          summary_list <- list()
                                          
                                          # Loop over each model combination
                                          for(j in 1:nrow(model_combinations)) {
                                            imp_model <- model_combinations$imp[j]
                                            subs_model <- model_combinations$subs[j]
                                            
                                            res <- apply_models(df = train_df, test_df = test_df, imp = imp_model, subs = subs_model)
                                            
                                            # Save summary results to disk immediately
                                            result_filename <- file.path(results_summary_folder, 
                                                                         paste0("result_scenario_", i, "model", j, ".rds"))
                                            saveRDS(res$Results_DataFrame, file = result_filename)
                                            
                                            # Save calibration data to disk immediately
                                            calibration_filename <- file.path(calibration_folder, 
                                                                              paste0("calibration_scenario_", i, "model", j, ".rds"))
                                            saveRDS(res$Calibration_Data, file = calibration_filename)
                                            
                                            # Add additional model and simulation info to the summary
                                            summary_df <- res$Results_DataFrame
                                            summary_df$imp <- imp_model
                                            summary_df$subs <- subs_model
                                            summary_df$scenario_index <- i
                                            summary_df$iteration <- iter
                                            summary_list[[j]] <- summary_df
                                          }
                                          
                                          # Clean up the generated data for this scenario to free memory
                                          rm(train_df, test_df)
                                          
                                          list(summary = do.call(rbind, summary_list))
                                        }
  
  # Stop the cluster to free resources
  parallel::stopCluster(cl)
  
  # Combine summary outputs for this iteration.
  final_summary_iter <- do.call(rbind, lapply(results_list_iter, function(x) x$summary))
  saveRDS(final_summary_iter, file = file.path(iter_folder, "final_summary_iter.rds"))
  
  cat("Iteration", iter, "completed and saved in", iter_folder, "\n")
}