library(ggplot2)
library(plyr)
library(reshape2)
library(RPostgreSQL)
require(hash)
library(Matrix)

hash_features <- hash()
reverse_hash_features <- hash()
hash_observations <- hash()


lookup_obs_num <- function(obs_id)
{
  return(hash_observations[[obs_id]])
}


lookup_feature_num <- function(feature_id)
{
  return(hash_features[[feature_id]])
}


lookup_gram_sequence <- function(feature_num)
{
  return(reverse_hash_features[[feature_num]])
}


prepare_data <- function()
 {
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
   
  statement <- paste("select bs.id, tg.gram_sequence, tg.frequency, bs.markedcategory
                      from two_grams tg, browsing_sessions bs
                      where bs.ClientIPServerIP = tg.ClientIPServerIP
                      and bs.BrowsingSessionID = tg.BrowsingSessionID
                      and bs.markedcategory in ('User', 'Bot')
                      order by bs.id, tg.gram_sequence", sep = "")
  res <- dbSendQuery(con, statement)
  data <- fetch(res, n = -1)

  features <- unique(data$gram_sequence)
  n_features <- length(features)
  hash_features <<- hash(features, 1:n_features)
  reverse_hash_features <<- hash(1:n_features, features)

  observations <- unique(data$id)
  n_observations <- length(observations)
  hash_observations <<- hash(observations, 1:n_observations)

  data$obs_num <- apply(data, 1, function(row)lookup_obs_num(row["id"]))
  data$feature_num <- apply(data, 1, function(row)lookup_feature_num(row["gram_sequence"]))
  sparse_mat <- sparseMatrix(i = data$obs_num, j = data$feature_num, x = data$frequency, dimnames=list(1:n_observations,1:n_features))
  cat(paste("nrow(sparse_mat) = ", nrow(sparse_mat), ", ncol(sparse_mat) = ", ncol(sparse_mat), "\n", sep = ""))

  session_labels <- unique(data[, c('id', 'markedcategory')]) 
  cat(paste("nrow(session_labels) = ", nrow(session_labels), "\n", sep = ""))

  dbDisconnect(con) 
  return(list("sparse_mat" = sparse_mat, "session_labels" = session_labels)) 
}

fit_model <- function()
{
  library(glmnet)
  all_data <- prepare_data()
  sparse_mat <- all_data[["sparse_mat"]]
  session_labels <- all_data[["session_labels"]]
  session_labels$category <- ifelse(session_labels$markedcategory == 'Bot', 1, 0)
  trg_model <- train_validate_test(sparse_mat, session_labels$category)
  return(trg_model)
 }


train_validate_test <- function(x, y)
{
  set.seed(1)
  train = sample(1:nrow(x), 0.5*nrow(x))
  test = (-train)
  y.test = y[test]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))

  #weights <- ifelse(y == 1, 6, 1)  
  cv.out = cv.glmnet(x[train, ], y[train], 
                     #weights = weights[train], 
                     alpha = 1, family="binomial", type.measure = "class")
  bestlam = cv.out$lambda.min

  #Get a table of training error, CV error and test error
  lambdas <- cv.out$lambda
  nlambda = length(lambdas) 
  trg_model <- glmnet(x[train, ], y[train], 
                      #weights = weights[train], 
                      family = "binomial", lambda = lambdas)

  prediction_on_trg <- predict(trg_model, newx = x[train, ], s = lambdas, type = "class")
  prediction_on_test <- predict(trg_model, newx = x[test, ], s = lambdas, type = "class")

  errors <- data.frame()
  for (i in 1:nlambda)
   {
     wrong_predictions_on_trg <- xor(y[train], as.numeric(prediction_on_trg[, i]))
     n_wrong_predictions_on_trg <- sum(wrong_predictions_on_trg)
     errors[i, "lambda"] <- lambdas[i]
     errors[i, "trg_error"] <- n_wrong_predictions_on_trg/length(y[train])

     v <- as.numeric(prediction_on_test[, i])
     wrong_predictions_on_test <- xor(y.test, v)
     n_wrong_predictions_on_test <- sum(wrong_predictions_on_test)
     errors[i, "test_error"] <- n_wrong_predictions_on_test/length(y.test)

     positives_on_test <- sum(y.test)
     false_negatives_on_test <- sum(as.numeric(y.test == 1 & v == 0))
     FNR <- false_negatives_on_test/positives_on_test
     errors[i, "FNR"] <- FNR

     negatives_on_test <- length(y.test) - sum(y.test)
     false_positives_on_test <- sum(as.numeric(y.test == 0 & v == 1))
     FPR <- false_positives_on_test/negatives_on_test
     errors[i, "FPR"] <- FPR

     errors[i, "cv_error"] <- cv.out$cvm[i]
     errors[i, "nonzero_covariates"] <- cv.out$nzero[i]
   }
  print(errors)
  write.csv(errors, "./documents/errors_glmnet/without_weights/errors_glmnet.csv")
}

analyze_glm_errors <- function()
{
  errors <- read.csv("./documents/errors_glmnet/without_weights/errors_glmnet.csv") 
  folder <- "./figures/errors_glmnet/without_weights/"
  filename <- paste(folder, "lambda_for_feature_selection.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")

  p <- ggplot(errors, aes(x = lambda, y = nonzero_covariates)) + geom_line(size=2) + 
        labs(x = "Lambda") + ylab("Number of features selected") + 
         theme(axis.text = element_text(colour = 'blue', size = 10)) +
         theme(axis.title = element_text(colour = 'red', size = 12))
  print(p)
  dev.off()
     
  filename <- paste(folder, "errors_glmnet.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")

  errors <- errors[, c("trg_error", "cv_error", "test_error", "nonzero_covariates", "FNR", "FPR")]
  df_long <- melt(errors, id = "nonzero_covariates")
  p <- ggplot(df_long, aes(x = nonzero_covariates, y = value, colour = variable)) + geom_line(size=2) + 
        labs(x = "Number of features selected") + ylab("Error") + 
         theme(axis.text = element_text(colour = 'blue', size = 10)) +
         theme(axis.title = element_text(colour = 'red', size = 12))
  print(p)
  dev.off()

  filename <- paste(folder, "errors_glmnet_zoomed.png", sep = "")
  png(filename,  width = 600, height = 480, units = "px")

  df_long <- subset(df_long, (nonzero_covariates <= 150)) 
  p <- ggplot(df_long, aes(x = nonzero_covariates, y = value, colour = variable)) + geom_line(size=2) + 
        labs(x = "Number of features selected") + ylab("Error") + 
         theme(axis.text = element_text(colour = 'blue', size = 10)) +
         theme(axis.title = element_text(colour = 'red', size = 12))
  print(p)
  dev.off()

  
}


