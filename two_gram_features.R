library(ggplot2)
library(plyr)
library(reshape2)
library(RPostgreSQL)
require(hash)
library(Matrix)
library(ggplot2)
library(plyr)
library(gridExtra)


gram_seq_in_sessions <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
  statement <- "select tg.gram_sequence, count(distinct bs.id)
                from two_grams tg, browsing_sessions bs
                where bs.ClientIPServerIP = tg.ClientIPServerIP
                and bs.BrowsingSessionID = tg.BrowsingSessionID
                and bs.markedcategory = 'User'
                group by tg.gram_sequence
                order by count(distinct bs.id) desc limit 30"
  res <- dbSendQuery(con, statement)
  user_frequencies <- fetch(res, n = -1)
  user_frequencies$gram_sequence <- factor(user_frequencies$gram_sequence, 
                              levels = user_frequencies$gram_sequence,
                              ordered = TRUE)

  gp1 <- ggplot(user_frequencies, aes(x = gram_sequence, y = count)) + geom_bar(stat="identity") + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.text.x = element_text(angle = 90)) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Number of user sessions 2grams appeared in")

  statement <- "select tg.gram_sequence, count(distinct bs.id)
                from two_grams tg, browsing_sessions bs
                where bs.ClientIPServerIP = tg.ClientIPServerIP
                and bs.BrowsingSessionID = tg.BrowsingSessionID
                and bs.markedcategory = 'Bot'
                group by tg.gram_sequence
                order by count(distinct bs.id) desc limit 30"
  res <- dbSendQuery(con, statement)
  bot_frequencies <- fetch(res, n = -1)
  bot_frequencies$gram_sequence <- factor(bot_frequencies$gram_sequence, 
                              levels = bot_frequencies$gram_sequence,
                              ordered = TRUE)

  gp2 <- ggplot(bot_frequencies, aes(x = gram_sequence, y = count)) + geom_bar(stat="identity") + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.text.x = element_text(angle = 90)) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Number of bot sessions 2grams appeared in")


  frame_grob <- grid.arrange(gp1, gp2, ncol = 1)
  grob <- grid.grab()

  image_file <- "./figures/gram_seq_in_sessions.png"
  png(image_file, width = 900, height = 900)
  grid.newpage()
  grid.draw(grob)
  dev.off()

  dbDisconnect(con)
}


gram_freq_in_sessions <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
  statement <- "select tg.gram_sequence, sum(tg.frequency)
                from two_grams tg, browsing_sessions bs
                where bs.ClientIPServerIP = tg.ClientIPServerIP
                and bs.BrowsingSessionID = tg.BrowsingSessionID
                and bs.markedcategory = 'User'
                group by tg.gram_sequence
                order by sum(tg.frequency) desc limit 30"
  res <- dbSendQuery(con, statement)
  user_frequencies <- fetch(res, n = -1)
  user_frequencies$gram_sequence <- factor(user_frequencies$gram_sequence, 
                              levels = user_frequencies$gram_sequence,
                              ordered = TRUE)

  gp1 <- ggplot(user_frequencies, aes(x = gram_sequence, y = sum)) + geom_bar(stat="identity") + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.text.x = element_text(angle = 90)) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Sum of frequencies of 2grams in user sessions")

  statement <- "select tg.gram_sequence, sum(tg.frequency)
                from two_grams tg, browsing_sessions bs
                where bs.ClientIPServerIP = tg.ClientIPServerIP
                and bs.BrowsingSessionID = tg.BrowsingSessionID
                and bs.markedcategory = 'Bot'
                group by tg.gram_sequence
                order by sum(tg.frequency) desc limit 30"
  res <- dbSendQuery(con, statement)
  bot_frequencies <- fetch(res, n = -1)
  bot_frequencies$gram_sequence <- factor(bot_frequencies$gram_sequence, 
                              levels = bot_frequencies$gram_sequence,
                              ordered = TRUE)

  gp2 <- ggplot(bot_frequencies, aes(x = gram_sequence, y = sum)) + geom_bar(stat="identity") + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.text.x = element_text(angle = 90)) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Sum of frequencies of 2grams in bot sessions")


  frame_grob <- grid.arrange(gp1, gp2, ncol = 1)
  grob <- grid.grab()

  image_file <- "./figures/gram_freq_in_sessions.png"
  png(image_file, width = 900, height = 900)
  grid.newpage()
  grid.draw(grob)
  dev.off()

  dbDisconnect(con)
}


gram_freq_in_sessions_for_pages <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
  statement <- "select tg.gram_sequence, sum(tg.frequency)
                from two_grams_with_pages tg, browsing_sessions bs
                where bs.ClientIPServerIP = tg.ClientIPServerIP
                and bs.BrowsingSessionID = tg.BrowsingSessionID
                and bs.markedcategory = 'User'
                group by tg.gram_sequence
                order by sum(tg.frequency) desc limit 30"
  res <- dbSendQuery(con, statement)
  user_frequencies <- fetch(res, n = -1)
  user_frequencies$gram_sequence <- factor(user_frequencies$gram_sequence, 
                              levels = user_frequencies$gram_sequence,
                              ordered = TRUE)

  gp1 <- ggplot(user_frequencies, aes(x = gram_sequence, y = sum)) + geom_bar(stat="identity") + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.text.x = element_text(angle = 90)) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Sum of frequencies of 2grams in user sessions")

  statement <- "select tg.gram_sequence, sum(tg.frequency)
                from two_grams_with_pages tg, browsing_sessions bs
                where bs.ClientIPServerIP = tg.ClientIPServerIP
                and bs.BrowsingSessionID = tg.BrowsingSessionID
                and bs.markedcategory = 'Bot'
                group by tg.gram_sequence
                order by sum(tg.frequency) desc limit 30"
  res <- dbSendQuery(con, statement)
  bot_frequencies <- fetch(res, n = -1)
  bot_frequencies$gram_sequence <- factor(bot_frequencies$gram_sequence, 
                              levels = bot_frequencies$gram_sequence,
                              ordered = TRUE)

  gp2 <- ggplot(bot_frequencies, aes(x = gram_sequence, y = sum)) + geom_bar(stat="identity") + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.text.x = element_text(angle = 90)) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Sum of frequencies of 2grams in bot sessions")


  frame_grob <- grid.arrange(gp1, gp2, ncol = 1)
  grob <- grid.grab()

  image_file <- "./figures/gram_freq_in_sessions_for_pages.png"
  png(image_file, width = 900, height = 900)
  grid.newpage()
  grid.draw(grob)
  dev.off()

  dbDisconnect(con)
}



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

prepare_data_page_sequence <- function()
 {
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
  
  #'NULL' is a special 2-gram for sessions which have not visited more than one page, and hence do not actually have any 2-gram of page URLs.
  #The frequency of a NULL 2-gram is always 0. 
  statement <- paste("select bs.id, COALESCE(tg.gram_sequence, 'NULL') as gram_sequence, COALESCE(tg.frequency, 0) as frequency, bs.markedcategory
                      from browsing_sessions bs left outer join two_grams_with_pages tg 
                      on (bs.ClientIPServerIP = tg.ClientIPServerIP
                      and bs.BrowsingSessionID = tg.BrowsingSessionID)
                      where bs.markedcategory in ('User', 'Bot')
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
  all_data <- prepare_data_page_sequence()
  sparse_mat <- all_data[["sparse_mat"]]
  session_labels <- all_data[["session_labels"]]
 
  #For SVM 1 and -1. For LR, 1 and 0.
  #session_labels$category <- ifelse(session_labels$markedcategory == 'Bot', 1, -1)

  #trg_model <- train_validate_test_lr(sparse_mat, session_labels$category)
  session_labels$markedcategory <- as.factor(session_labels$markedcategory)
  
  #trg_model <- svm_training_only(sparse_mat, session_labels$markedcategory)
  #return(trg_model)
  tune.out <- train_validate_test_svm(sparse_mat, session_labels$markedcategory)
  #tune.out <- svm_on_balanced_sample(sparse_mat, session_labels$markedcategory)
 }


train_validate_test_lr <- function(x, y)
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
     #result <- data.frame(y = y[train], prediction_on_trg = as.numeric(prediction_on_trg[, i]))
     #print(result)
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
  #Min CV error = 0.1451613. Corresponding lambda = 0.004979912, training error = 0.002304147, test error = 0.09885057, 
  #FNR = 0.1810345, FPR = 0.06896552, #covariates = 336 out of 26370 (1.2% of covariates).
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

svm_training_only <- function(x, y)
{
  library(e1071)
  set.seed(1)

  model <- svm(x, y, type = "C-classification", kernel = "linear")
  #model <- svm(x, y, kernel = "radial", gamma = 1, cost = 1)
  #model <- svm(x, y, kernel = "polynomial", degree = 3)
  predicted_label <- predict(model, x)

  wrong_predictions_on_trg <- as.numeric(y != predicted_label)
  n_wrong_predictions_on_trg <- sum(wrong_predictions_on_trg)
  trg_error <- n_wrong_predictions_on_trg/length(y)
  #Training error with linear kernel is 0.0023. FNR = 0, FPR = 0.00316
  #Training error with RBF kernel is 0, so FNR = 0, FPR = 0. RBF is overfitting.
  #Training error with polynomial kernel with degree 3 is 0.207, FNR = 0.7457, FPR = 0.0063. 
  cat(paste("n_wrong_predictions_on_trg = ", n_wrong_predictions_on_trg, ", length(y) = ", length(y), ", trg_error = ", trg_error, "\n", sep = ""))
  print(table(y, predicted_label, dnn = list('actual', 'predicted')))
  model
}

train_validate_test_svm <- function(x, y)
{
  library(e1071)
  set.seed(1)
  train = sample(1:nrow(x), 0.5*nrow(x))
  test = (-train)
  y.test = y[test]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))

  tune.out = tune.svm(x[train, ], y[train], kernel = "linear", class.weights = c(Bot = 2.68), cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100))
  #tune.out = tune.svm(x[train, ], y[train], kernel = "radial", cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4))
  #tune.out = tune.svm(x[train, ], y[train], kernel = "polynomial", cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4))
  bestmod <- tune.out$best.model
  #plot(bestmod, x[train, ])
  ypred = predict(bestmod, x[test, ])

  #With best model from CV applied on test data for linear kernel, FNR = 0.1, FPR = 0.056, test error = 0.06896. Best CV error = 0.06676022 for cost = 0.1
  #With best model from CV applied on test data for RBF kernel, FNR = 0.706, FPR = 0, test error = 0.1885057. Best CV error = 0.2391649 for gamma = 0.5 and cost = 10
  #With best model from CV applied on test data for polynomial kernel, FNR = 0.6724, FPR = 0.01567, test error = 0.19. Best CV error = 0.2138478 for degree = 2 and cost = 100
  
  print(table(y.test, ypred, dnn = list('actual', 'predicted')))
  data_for_plots <- false_negative_analysis(x[test, ], y.test, ypred)
  tune.out
 }

false_negative_analysis <- function(x.test, y.test, ypred)
{
  length_test <- length(y.test)
  false_negative_indexes <- (1:length_test)[y.test == 'Bot' & ypred == 'User']
  false_negatives <- x.test[false_negative_indexes, ]
  cat(paste("nrow(false_negatives) = ", nrow(false_negatives), "\n", sep = ""))
  two_gram_total_frequencies <- colSums(false_negatives, nrow(false_negatives), ncol(false_negatives))
  columns <- colnames(x.test)
  two_grams_with_nz_freq <- columns[(1:ncol(x.test))[two_gram_total_frequencies > 0]]
  nz_frequencies <- two_gram_total_frequencies[two_gram_total_frequencies > 0]
  data_for_plots <- data.frame(two_grams_with_nz_freq = two_grams_with_nz_freq, nz_frequencies = nz_frequencies)
  data_for_plots$two_grams_with_nz_freq <- apply(data_for_plots, 1, function(row)lookup_gram_sequence(as.character(row["two_grams_with_nz_freq"])))
  data_for_plots <- data_for_plots[order(-data_for_plots[,"nz_frequencies"]),]

  data_for_plots$two_grams_with_nz_freq <- factor(data_for_plots$two_grams_with_nz_freq, 
                                                  levels = data_for_plots$two_grams_with_nz_freq,
                                                  ordered = TRUE)
  data_for_plots <- data_for_plots[1:20, ]

  p <- ggplot(data_for_plots, aes(x = two_grams_with_nz_freq, y = nz_frequencies)) + geom_bar(stat="identity") + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.text.x = element_text(angle = 90)) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Sum of frequencies of false negative 2grams")

  image_file <- "./figures/gram_freq_in_false_negatives.png"
  png(image_file, width = 900, height = 900)
  print(p)
  dev.off()
}

#We take samples from the User class so that the number of bot and user sessions become same, and train SVM on that.
svm_on_balanced_sample <- function(x, y)
{
  library(e1071)
  set.seed(1)

  train = sample(1:nrow(x), 0.8*nrow(x))
  test = (-train)

  x.train = x[train, ]
  y.train = y[train]
  x.test = x[test, ]
  y.test = y[test]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))

  #Take the bot and user sessions apart from the training data, do the sampling on the user subset, and append them back.
  #Note: bot_indices_in_training is wrt the indices in y.train and not wrt the indices in y
  bot_indices_in_training <- which(y.train == 'Bot')
  y.train.bot <- y.train[bot_indices_in_training]

  #Note: user_indices_in_training is wrt the indices in y.train and not wrt the indices in y
  user_indices_in_training <- which(y.train == 'User')
  sample_user_indices_in_training <- sample(user_indices_in_training, length(bot_indices_in_training))
  y.train.user <- y.train[sample_user_indices_in_training]


  x.train <- x.train[append(sample_user_indices_in_training, bot_indices_in_training), ]
  y.train <- y.train[append(sample_user_indices_in_training, bot_indices_in_training)]
  
  cat(paste("Size of training data = ", nrow(x.train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))

  tune.out = tune.svm(x.train, y.train, kernel = "linear", cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100))
  bestmod <- tune.out$best.model
  ypred = predict(bestmod, x.test)

  #With best model from CV applied on test data for linear kernel, FNR = 0.1, FPR = 0.056, test error = 0.06896. Best CV error = 0.06676022 for cost = 0.1
  print(table(y.test, ypred, dnn = list('actual', 'predicted')))
  tune.out
}


df <- data.frame()

populate_data_frame <- function(obs_id, feature_id, frequency, markedcategory)
{
  df[obs_id, feature_id] <<- frequency
  df[obs_id, "markedcategory"] <<- markedcategory
}

#Goal: Prepare data with selected features only
prepare_data_post_feature_selection <- function(how_many = 30)
{
   con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
   gs <- read.csv("/Users/blahiri/hiddenmarkovmodel/documents/gram_seq_for_feature_sel.csv")
   gs <- gs[1:how_many, ]
   clause <- paste("('", paste(gs$gram_sequence, collapse = "', '"), "')", sep = "")
   
   statement <- paste("select bs.id, tg.gram_sequence, tg.frequency, bs.markedcategory
                      from three_grams tg, browsing_sessions bs
                      where bs.ClientIPServerIP = tg.ClientIPServerIP
                      and bs.BrowsingSessionID = tg.BrowsingSessionID
                      and bs.markedcategory in ('User', 'Bot')
                      and tg.gram_sequence in ", clause,
                      " order by bs.id, tg.gram_sequence", sep = "")
  res <- dbSendQuery(con, statement)
  data <- fetch(res, n = -1)
  

  features <- unique(data$gram_sequence)
  n_features <- length(features)

  observations <- unique(data$id)
  n_observations <- length(observations)
  n_data <- nrow(data)

  #Create a data frame with sessions as observations and 2-grams as features
  df <<- data.frame(matrix(nrow = n_observations, 
                          ncol = n_features + 1))
  rownames(df) <<- observations
  colnames(df) <<- append(features, 'markedcategory')

  apply(data, 1, function(row)populate_data_frame(row["id"], row["gram_sequence"], row["frequency"], row["markedcategory"]))
  df[is.na(df)] <- 0

  #If there are sessions that are not being selected because the seleted feature 2-grams do not appear in them, pick up and 
  #just append them with all 0's
  statement <- paste("select distinct bs.id, bs.markedcategory
                      from browsing_sessions bs
                      where bs.markedcategory in ('User', 'Bot')
                      and not exists (select 1 from two_grams tg where bs.ClientIPServerIP = tg.ClientIPServerIP
                                      and bs.BrowsingSessionID = tg.BrowsingSessionID
                                      and tg.gram_sequence in ", clause, ") ", 
                      "order by bs.id", sep = "")
  res <- dbSendQuery(con, statement)
  remaining_sessions <- fetch(res, n = -1)
  n_remaining_sessions <- nrow(remaining_sessions)
 
  rownames(df[(n_observations + 1):(n_observations + n_remaining_sessions), ]) <- remaining_sessions$id
  df[(n_observations + 1):(n_observations + n_remaining_sessions), "markedcategory"] <- remaining_sessions$markedcategory
  df[(n_observations + 1):(n_observations + n_remaining_sessions), features] <- 0

  #df[, features] <- as.numeric(df[, features])
  write.csv(df, "/Users/blahiri/hiddenmarkovmodel/documents/prepared_data_post_feature_selection.csv")
  dbDisconnect(con)
  df
}

cont_table_for_two_gram <- function(con, gram_sequence)
{
  statement <- paste("select bs.markedcategory, count(distinct bs.id)
                      from two_grams tg, browsing_sessions bs
                      where bs.ClientIPServerIP = tg.ClientIPServerIP
                      and bs.BrowsingSessionID = tg.BrowsingSessionID
                      and bs.markedcategory in ('User', 'Bot')
                      and tg.gram_sequence = '", gram_sequence, "' ",  
                      "group by bs.markedcategory
                      order by bs.markedcategory", sep = "")
  res <- dbSendQuery(con, statement)
  gram_present <- fetch(res, n = -1)
  botrow <- subset(gram_present, (markedcategory == 'Bot'))
  a1 <- ifelse(nrow(botrow) > 0, botrow[, "count"], 0)
  
  userrow <- subset(gram_present, (markedcategory == 'User'))
  a2 <- ifelse(nrow(userrow) > 0, userrow[, "count"], 0)

  statement <- paste("select bs.markedcategory, count(*)
                      from browsing_sessions bs
                      where bs.markedcategory in ('User', 'Bot')
                      and not exists (select 1 from two_grams tg
                                      where bs.ClientIPServerIP = tg.ClientIPServerIP
                                      and bs.BrowsingSessionID = tg.BrowsingSessionID
                                      and bs.markedcategory in ('User', 'Bot')
                                      and tg.gram_sequence = '", gram_sequence, "') ", 
                                      "group by bs.markedcategory 
                                      order by bs.markedcategory", sep = "")
  res <- dbSendQuery(con, statement)
  gram_absent <- fetch(res, n = -1)
  botrow <- subset(gram_absent, (markedcategory == 'Bot'))
  a3 <- ifelse(nrow(botrow) > 0, botrow[, "count"], 0)
  userrow <- subset(gram_absent, (markedcategory == 'User'))
  a4 <- ifelse(nrow(userrow) > 0, userrow[, "count"], 0)
  return(c(a1, a2, a3, a4))
}

prepare_data_for_feature_selection <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
  statement <- paste("select distinct gram_sequence
                      from two_grams tg, browsing_sessions bs
                      where bs.ClientIPServerIP = tg.ClientIPServerIP
                      and bs.BrowsingSessionID = tg.BrowsingSessionID
                      and bs.markedcategory in ('User', 'Bot')", sep = "")
  res <- dbSendQuery(con, statement)
  gs <- fetch(res, n = -1)
  n_gs <- nrow(gs)

  for (i in 1:n_gs)
  {
    v <- cont_table_for_two_gram(con, gs[i, "gram_sequence"])
    gs[i, "a1"] <- v[1]
    gs[i, "a2"] <- v[2]
    gs[i, "a3"] <- v[3]
    gs[i, "a4"] <- v[4]
    if (i %% 200 == 0)
    {
      cat(paste("i = ", i, ", time = ", Sys.time(), "\n", sep = ""))
    }
  }
  dbDisconnect(con)
  gs
}

#Given a vector of frequencies of diff categories, computes the entropy.
my_entropy <- function(x)
{
  p <- x/sum(x)
  len <- length(p)
  sum <- 0
  for (i in 1:len)
  {
    if (p[i] > 0)
    {
      sum <- sum - p[i]*log(p[i], 2)
    }
  }
  sum
}

info_gain_for_gram_seq <- function(entropy_session_category, a1, a2, a3, a4)
{
  spec_cond_entropy_gram_present <- my_entropy(c(a1, a2))
  spec_cond_entropy_gram_absent <- my_entropy(c(a3, a4))
  prob_gram_present <- (a1 + a2)/(a1 + a2 + a3 + a4)
  prob_gram_absent <- (a3 + a4)/(a1 + a2 + a3 + a4)
  cond_entropy <- prob_gram_present*spec_cond_entropy_gram_present + prob_gram_absent*spec_cond_entropy_gram_absent
  return(entropy_session_category - cond_entropy)
}

compute_info_gain <- function()
{
  entropy_session_category <- my_entropy(c(236, 633))
  gs <- read.csv("/Users/blahiri/hiddenmarkovmodel/documents/gram_seq_for_feature_sel.csv")
  gs$info_gain <- apply(gs, 1, function(row)info_gain_for_gram_seq(entropy_session_category, as.numeric(row["a1"]), as.numeric(row["a2"]), 
                        as.numeric(row["a3"]), as.numeric(row["a4"])))
  gs <- gs[order(-gs[,"info_gain"]),]
  write.csv(gs, "/Users/blahiri/hiddenmarkovmodel/documents/gram_seq_for_feature_sel.csv")
}

rf_with_selected_features <- function(n_features = 30)
{
  library(randomForest)
  set.seed(1)
  df <- prepare_data_post_feature_selection(n_features)
  df[,"markedcategory"] <- as.factor(df[,"markedcategory"])

  ubs.rf <- randomForest(df[,!(names(df) %in% c("markedcategory"))], df[,"markedcategory"], 
               prox = TRUE)

  df$predicted <-  ubs.rf$predicted
  result <- table(df[,"markedcategory"], df[, "predicted"], dnn = list('actual', 'predicted'))
  print(result)
  overall_error <- (result[1,2] + result[2, 1])/sum(result)
  FNR <- result[1,2]/sum(result[1,])
  FPR <- result[2,1]/sum(result[2,])
  cat(paste("overall_error = ", overall_error, ", FNR = ", FNR, ", FPR = ", FPR, "\n", sep = ""))
}

ctree_with_selected_features <- function(n_features = 30)
{
  library(party)
  #df <- prepare_data_post_feature_selection(n_features)
  df <- read.csv("/Users/blahiri/hiddenmarkovmodel/documents/prepared_data_post_feature_selection.csv")
  df[,"markedcategory"] <- factor(df[,"markedcategory"])
  #df$markedcategory <- as.numeric(df$markedcategory == 'Bot')
  print(class(df[,"markedcategory"])) 
  #rownames(df) <- 1:nrow(df)
  print(df[1:5, ])
  ubs.ct <- ctree(markedcategory ~ ., data = df) 

  df$predicted <-  predict(ubs.ct) 
  result <- table(df[,"markedcategory"], df[, "predicted"], dnn = list('actual', 'predicted'))
  print(result)
  overall_error <- (result[1,2] + result[2, 1])/sum(result)
  FNR <- result[1,2]/sum(result[1,])
  FPR <- result[2,1]/sum(result[2,])
  cat(paste("overall_error = ", overall_error, ", FNR = ", FNR, ", FPR = ", FPR, "\n", sep = ""))
} 
 
svm_with_selected_features <- function(n_features = 30)
{
  #df <- prepare_data_post_feature_selection(n_features)
  df <- read.csv("/Users/blahiri/hiddenmarkovmodel/documents/prepared_data_post_feature_selection.csv", header = TRUE)
  df <- df[-1]
  tune.out <- train_validate_test_svm(df[,!(names(df) %in% c("markedcategory"))], df[,"markedcategory"])  
} 


gbm_with_selected_features <- function(n_features = 30)
{
  library(gbm)
  library(pracma)
  set.seed(1)
  #df <- prepare_data_post_feature_selection(n_features)
  df <- read.csv("/Users/blahiri/hiddenmarkovmodel/documents/prepared_data_post_feature_selection.csv", header = TRUE)
  df <- df[-1]
  df$markedcategory <- as.numeric(df$markedcategory == 'Bot')
  train = sample(1:nrow(df), nrow(df)/2)
  cat(paste("training data size = ", length(train), ", test data size = ", nrow(df[-train, ]), "\n", sep = ""))
  #boost.ubs <- gbm(markedcategory ~ ., data = df[train, ], distribution = "bernoulli", n.trees = 5000, interaction.depth = 1)

  boost.ubs <- gbm.fit(df[train,!(names(df) %in% c("markedcategory"))], df[train,"markedcategory"], distribution = "bernoulli", n.trees = 5000, interaction.depth = 2)
  yhat.boost = predict(boost.ubs, newdata = df[-train, ], n.trees = 5000)
  #Responses are on log odds scale, so take the sigmoid function to get the probabilities of positive class back
  yhat <- sigmoid(yhat.boost)
  yhat <- as.numeric(yhat >= 0.5)

  result <- table(df[-train,"markedcategory"], yhat, dnn = list('actual', 'predicted'))
  print(result)
  overall_error <- (result[1,2] + result[2, 1])/sum(result)
  FPR <- result[1,2]/sum(result[1,])
  FNR <- result[2,1]/sum(result[2,])
  cat(paste("overall_error = ", overall_error, ", FNR = ", FNR, ", FPR = ", FPR, "\n", sep = ""))
  #yhat.boost
  boost.ubs
} 

