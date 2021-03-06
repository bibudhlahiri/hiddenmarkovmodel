library(randomForest)
library(RPostgreSQL)
library(e1071)
library(nnet)
library(rpart)
library(ggplot2)
library(plyr)
library(pmml)

prepare_data_user_behavior <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
  statement <- "select bs.id, bs.MarkedCategory, max(hr.timestamp) - min(hr.timestamp) as duration_seconds
                from browsing_sessions bs, http_requests hr
                where bs.ClientIPServerIP = hr.ClientIPServerIP
                and bs.browsingsessionid = hr.browsingsessionid
                and bs.MarkedCategory in ('User', 'Bot')
                group by bs.id, bs.MarkedCategory
                order by bs.id"
  res <- dbSendQuery(con, statement)
  sess_durations <- fetch(res, n = -1) 

  statement <- "select bs.id, count(*) n_pages_from_session
                from browsing_sessions bs, http_requests_pages hr
                where bs.ClientIPServerIP = hr.ClientIPServerIP
                and bs.browsingsessionid = hr.browsingsessionid
                and bs.MarkedCategory in ('User', 'Bot')
                group by bs.id"
  res <- dbSendQuery(con, statement)
  sess_pages <- fetch(res, n = -1)
  

  sessions <- merge(sess_durations, sess_pages, all.x = TRUE) 


  statement <- "select bs.id, count(distinct hr.completeurl) n_distinct_pages_from_session
                from browsing_sessions bs, http_requests_pages hr
                where bs.ClientIPServerIP = hr.ClientIPServerIP
                and bs.browsingsessionid = hr.browsingsessionid
                and bs.MarkedCategory in ('User', 'Bot')
                group by bs.id
                order by n_distinct_pages_from_session"
  res <- dbSendQuery(con, statement)
  sess_distinct_pages <- fetch(res, n = -1)

  sessions <- merge(sessions, sess_distinct_pages, all.x = TRUE)
  sessions[is.na(sessions)] <- 0
  #print(sessions)
  sessions[,"markedcategory"] <- as.factor(sessions[,"markedcategory"])
  dbDisconnect(con)
  cat(paste("nrow(sessions) = ", nrow(sessions), "\n", sep = ""))
  return(sessions)
}

#Get the frequencies of 2-grams of pages for the (539) sessions which visit 2 or more pages
prepare_data_page_sequence <- function()
 {
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
 
  #bs.id is the concatenation of bs.ClientIPServerIP and bs.browsingsessionid
  statement <- paste("select bs.id, tg.gram_sequence, tg.frequency
                      from browsing_sessions bs inner join two_grams_with_pages tg 
                      on (bs.ClientIPServerIP = tg.ClientIPServerIP
                      and bs.BrowsingSessionID = tg.BrowsingSessionID)
                      where bs.markedcategory in ('User', 'Bot')
                      order by bs.id, tg.gram_sequence", sep = "")
  res <- dbSendQuery(con, statement)
  data <- fetch(res, n = -1)
  n_data <- nrow(data)

  nrows <- length(unique(data$id))
  ncols <- length(unique(data$gram_sequence))
  data_matrix <- mat.or.vec(nrows, ncols)
  rownames(data_matrix) <- unique(data$id)
  colnames(data_matrix) <- unique(data$gram_sequence)
  
  for (i in 1:n_data)
  {
    data_matrix[data[i, "id"], data[i, "gram_sequence"]] <- data[i, "frequency"]
  }
  cat(paste("nrow(data_matrix) = ", nrow(data_matrix), ", ncol(data_matrix) = ", ncol(data_matrix), "\n", sep = ""))
  #print(data_matrix)
  dbDisconnect(con) 
  return(data.frame(data_matrix))
}

create_table_script_for_page_sequence_matrix <- function()
{
  page_sequence <- prepare_data_page_sequence()
  columns <- colnames(page_sequence)
  statement <- "create table page_sequence_matrix (session_id varchar, "
  for (column in columns)
  {
    statement <- paste(statement, column, " integer, " , sep = "") 
  }
  statement <- substring(statement, 1, nchar(statement) - 2)
  statement <- paste(statement, ")", sep = "")
  sink("create_table_script_for_page_sequence_matrix.sql")
  cat(statement)
  sink()
}


prepare_data <- function()
{
  #page_sequence <- prepare_data_page_sequence()
  page_sequence <- read.csv("/Users/blahiri/hiddenmarkovmodel/documents/prepared_data_post_feature_selection.csv")
  user_behavior <- prepare_data_user_behavior()

  sessions <- merge(user_behavior, page_sequence, all.x = TRUE)
  sessions[is.na(sessions)] <- 0
  cat(paste("nrow(sessions) = ", nrow(sessions), ", ncol(sessions) = ", ncol(sessions), "\n", sep = ""))
  #print(colnames(sessions))
  sessions
}



#Classification with simple attributes like number of pages visited in session, duration of session, etc
classify_rf <- function()
{
  set.seed(1)
  sessions <- prepare_data()
  #Random forest does not need splitting data into training and test sets, or cross-validation, as the prediction on a point 
  #is based on trees (about 1/3rd) where it is out-of-bag. The trees differ in two things: each tree takes a bootstrap sample of 
  #the training data, and picks mtry features at random to build the tree.
  sessions.rf <- randomForest(sessions[,!(names(sessions) %in% c("id", "markedcategory"))], 
                              sessions[,"markedcategory"], 
                              prox = TRUE)

  sessions$predicted <-  sessions.rf$predicted
  print(table(sessions[,"markedcategory"], sessions[, "predicted"], dnn = list('actual', 'predicted')))
  return(sessions.rf) 
}

train_validate_test_rf <- function()
 {
   set.seed(1)
   sessions <- prepare_data()
   train = sample(1:nrow(sessions), 0.5*nrow(sessions))
   test = (-train)
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(sessions) - length(train)), "\n", sep = ""))

   #tune.out = tune.randomForest(markedcategory ~ duration_seconds + n_pages_from_session + n_distinct_pages_from_session, 
   #                             data = sessions[train, ], ntree = seq(100, 600, 100), mtry = c(1, 2))
   tune.out = tune.randomForest(sessions[,!(names(sessions) %in% c("id", "markedcategory"))], 
                                sessions[,"markedcategory"], 
                                ntree = seq(100, 600, 100), mtry = c(1, 2))
   bestmod <- tune.out$best.model
   ypred <- predict(bestmod, newdata = sessions[test, ], type = "class")
   print(table(sessions[test, "markedcategory"], ypred, dnn = list('actual', 'predicted')))
   tune.out
 }

#x_i ia a support vector, n is a test data point.
compute_linear_kernel <- function(x_i, n)
{
  inner_prod <- x_i %*% n
  return(inner_prod)
}


#x_i ia a support vector, n is a test data point.
compute_rbf_kernel <- function(gamma, x_i, n)
{
  inner_prod <- exp(-gamma*sum((x_i - n)^2))
  return(inner_prod)
}

predict_test_point_svm_rbf <- function(tot.nSV, coefs, gamma, SV, rho, n)
{
  sum <- 0
  for (i in 1:tot.nSV)
  {
    kern_val <- compute_rbf_kernel(gamma, SV[i, ], n)
    sum <- sum + coefs[i]*kern_val
  }
  return(sign(sum - rho))
}

predict_test_point_svm_linear <- function(tot.nSV, coefs, SV, rho, n)
{
  sum <- 0
  for (i in 1:tot.nSV)
  {
    kern_val <- compute_linear_kernel(SV[i, ], n)
    sum <- sum + coefs[i]*kern_val
  }
  return(sign(sum - rho))
}


train_validate_test_svm <- function()
{
  set.seed(1)
  sessions <- prepare_data()
  #x <- sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")]
  x <- sessions[,!(names(sessions) %in% c("id", "markedcategory"))]
  y <- sessions[,"markedcategory"]
  train = sample(1:nrow(x), 0.5*nrow(x))

  test = (-train)
  y.test = y[test]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))
  tab <- table(y[train])
  bot_class_weight <- as.numeric(tab["User"]/tab["Bot"])
  cat(paste("bot_class_weight = ", bot_class_weight, "\n", sep = ""))
 
  #tune.out = tune.svm(x[train, ], y[train], kernel = "linear", 
  #                    class.weights = c(Bot = bot_class_weight) , cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100))
  tune.out = tune.svm(x[train, ], y[train], kernel = "radial", 
                      class.weights = c(Bot = bot_class_weight), 
                      cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), gamma = c(0.125, 0.25, 0.5, 1, 2, 3, 4, 5)
                      )
  bestmod <- tune.out$best.model

  ypred = predict(bestmod, x[train, ])
  cat("Confusion matrix for training data\n")
  print(table(y[train], ypred, dnn = list('actual', 'predicted')))

  cat("Confusion matrix for test data\n")
  #ypred = predict(bestmod, x[test, ])

  #Custom method for predicting labels on test data
  coefs <- bestmod$coefs
  rho <- bestmod$rho
  gamma <- bestmod$gamma
  tot.nSV <- bestmod$tot.nSV
  SV <- bestmod$SV

  x.test <- x[test, ]
 
  #Got this by e1071:::predict.svm at R command prompt
  if (any(bestmod$scaled)) 
  {
     x.test[, bestmod$scaled] <- scale(x.test[, bestmod$scaled, drop = FALSE], center = bestmod$x.scale$"scaled:center", 
                                       scale = bestmod$x.scale$"scaled:scale")
  }
  ypred <- apply(x.test, 1, function(row)predict_test_point_svm_rbf(tot.nSV, coefs, gamma, SV, rho, row)) 
  ypred <- ifelse(ypred == -1, 'Bot', 'User')
  
  print(table(y.test, ypred, dnn = list('actual', 'predicted')))
  tune.out
 }

 

 train_validate_test_knn <- function()
 {
   set.seed(1)
   sessions <- prepare_data()
   #x <- sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")]
   x <- sessions[,!(names(sessions) %in% c("id", "markedcategory"))]
   y <- sessions[,"markedcategory"]
   train = sample(1:nrow(x), 0.5*nrow(x))
   test = (-train)
   y.test = y[test]
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))

   tune.out = tune.knn(x[train, ], y[train], k = 1:10)
   best_k <- tune.out$best.model$k
   ypred <- knn(x[train, ], x[test, ], y[train], k = best_k)
   print(table(y.test, ypred, dnn = list('actual', 'predicted')))
   tune.out
 }

#n is the test data point
predict_test_point_rpart <- function(model, test_point)
{
  frame <- model$frame
  splits <- model$splits
  current_node_number <- 1
  rownames_split_matrix <- rownames(splits)
  n_splits <- nrow(splits)

  while (TRUE)
  {
    current_node <- frame[as.character(current_node_number), ]
    current_node_split_var <- as.character(current_node[1, "var"])
    if (as.character(current_node_split_var) == '<leaf>')
    {
      predicted <- current_node[1, "yval"]
      break
    }
    current_node_size <- current_node[1, "n"]
    current_node_split_point <- 0
    current_node_direction <- 0
    
    #Find the split point by scrolling the splits matrix, look for row that matches
    #on current_node_split_var and current_node_size
    
    for (j in 1:n_splits)
    {
      if ((rownames_split_matrix[j] ==  current_node_split_var) & 
          (splits[j, "count"] == current_node_size))
      {
        current_node_split_point <- splits[j, "index"]
        current_node_direction <- splits[j, "ncat"]
        break
      }
    }
    if (current_node_direction == 1)
    {
      # x >= cutpoint is sent to the left, otherwise to the right
      if (test_point[1, current_node_split_var] >= current_node_split_point)
      {
        current_node_number <- 2*current_node_number
      }
      else
      {
        current_node_number <- 2*current_node_number + 1
      }
    }
    else
    {
      # x < cutpoint is sent to the left, otherwise to the right
      if (test_point[1, current_node_split_var] < current_node_split_point)
      {
        current_node_number <- 2*current_node_number
      }
      else
      {
        current_node_number <- 2*current_node_number + 1
      }
    }
  } #end while
  predicted <- ifelse(predicted == 1, 'Bot', 'User')
}

train_validate_test_rpart <- function()
 {
   set.seed(1)
   sessions <- prepare_data()
   train = sample(1:nrow(sessions), 0.5*nrow(sessions))
   test = (-train)
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(sessions) - length(train)), "\n", sep = ""))

   #tune.out = tune.rpart(markedcategory ~ duration_seconds + n_pages_from_session + n_distinct_pages_from_session, data = sessions[train, ], minsplit = c(5, 10, 15), maxdepth = c(1, 3, 5, 7))
   str_formula <- "markedcategory ~ "
   for (column in colnames(sessions))
   {
     if (column != 'id' & column != 'markedcategory')
     {
       str_formula <- paste(str_formula, column, " + ", sep = "")
     }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   #print(str_formula)
   
   tune.out = tune.rpart(as.formula(str_formula), data = sessions[train, ], minsplit = c(5, 10, 15), maxdepth = c(1, 3, 5, 7))
   bestmod <- tune.out$best.model
   
   #ypred <- predict(bestmod, newdata = sessions[test, ], type = "class")
    
   ypred <- apply(sessions[test, !(names(sessions) %in% c("id", "markedcategory"))], 1, function(row)predict_test_point_rpart(bestmod, data.frame(t(row)))) 
   print(table(sessions[test, "markedcategory"], ypred, dnn = list('actual', 'predicted')))

   tune.out
 }

sigmoid <- function(x)
{
  return(1/(1 + exp(-x)))
}

prepare_connection_weights <- function(model)
{
  wts <- model$wts
  n <- model$n
  n_input_units <- n[1]
  n_hidden_units <- n[2]
  n_output_units <- n[3]

  input_to_hidden <- mat.or.vec(n_hidden_units, n_input_units + 1)
  counter <- 1
  for (i in 1:n_hidden_units)
  {
    for (j in 1:(n_input_units + 1))
    {
      #The element at the i-th row and j-th column in the matrix is the weight of the connection comming from 
      #the j-th unit in the previous (input) layer to the i-th unit in the current (hidden) layer
      input_to_hidden[i, j] <- wts[counter]
      counter <- counter + 1
    }
  } 
  hidden_to_output <- mat.or.vec(n_output_units, n_hidden_units + 1)
  for (i in 1:n_output_units)
  {
    for (j in 1:(n_hidden_units + 1))
    {
      #The element at the i-th row and j-th column in the matrix is the weight of the connection comming from 
      #the j-th unit in the previous (hidden) layer to the i-th unit in the current (output) layer
      hidden_to_output[i, j] <- wts[counter]
      counter <- counter + 1
    }
  }
  cat("input_to_hidden\n")
  print(input_to_hidden)
  cat("hidden_to_output\n")
  print(hidden_to_output)
  return(list("input_to_hidden" = input_to_hidden, "hidden_to_output" = hidden_to_output))
}

predict_test_point_nn <- function(input_to_hidden, hidden_to_output, test_point)
{
  test_point <- append(1, as.numeric(test_point))
  activation_values_hidden_layer <- sigmoid(input_to_hidden%*%test_point)

  activation_values_hidden_layer <- append(1, activation_values_hidden_layer)
  activation_value_output_layer <- sigmoid(hidden_to_output%*%activation_values_hidden_layer)
  #Internally, 1 stands for User, 0 for Bot
  predicted <- ifelse(activation_value_output_layer >= 0.5, 'User', 'Bot')
}


train_validate_test_nn_single_hidden_layer <- function()
 {
   set.seed(1)
   sessions <- prepare_data()
   #x <- sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")]
   x <- sessions[,!(names(sessions) %in% c("id", "markedcategory"))]
   y <- sessions[,"markedcategory"]
   train = sample(1:nrow(x), 0.5*nrow(x))
   test = (-train)
   y.test = y[test]
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(sessions) - length(train)), "\n", sep = ""))

   #tune.out = tune.nnet(markedcategory ~ duration_seconds + n_pages_from_session + n_distinct_pages_from_session, data = sessions[train, ], size = seq(2, 12, 2)) 
   str_formula <- "markedcategory ~ "
   for (column in colnames(sessions))
   {
     if (column != 'id' & column != 'markedcategory')
     {
       str_formula <- paste(str_formula, column, " + ", sep = "")
     }
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   tune.out = tune.nnet(as.formula(str_formula), data = sessions[train, ], size = seq(2, 12, 2))
   
   bestmod <- tune.out$best.model
   
   #ypred <- predict(bestmod, newdata = sessions[test, ], type = "class")
   
   connection_weights <- prepare_connection_weights(bestmod)
   input_to_hidden <- connection_weights[["input_to_hidden"]]
   hidden_to_output <- connection_weights[["hidden_to_output"]]
   ypred <- apply(x[test, ], 1, function(row)predict_test_point_nn(input_to_hidden, hidden_to_output,  data.frame(t(row))))
   print(table(y.test, ypred, dnn = list('actual', 'predicted')))
 }

#A different implementation of neural network. TODO: Have to check how prediction works.
train_validate_test_nn <- function()
 {
   set.seed(1)
   sessions <- prepare_data()
   x <- sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")]
   y <- sessions[,"markedcategory"]
   y <- ifelse(y == 'Bot', 1, 0)
   train = sample(1:nrow(x), 0.5*nrow(x))
   test = (-train)
   y.test = y[test]
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(sessions) - length(train)), "\n", sep = ""))

   #bots_ann = nnet(x[train, ], class.ind(y[train]), size = 2)
   bots_ann <- neuralnet(markedcategory ~ duration_seconds + n_pages_from_session + n_distinct_pages_from_session, data = sessions[train, ]) 
   ypred <- predict(bots_ann, x[test, ], type = "class")
   
   #bestmod <- tune.out$best.model
   #ypred <- predict(bestmod, newdata = sessions[test, ], type = "class")
   print(ypred)
   print(table(y.test, ypred, dnn = list('actual', 'predicted')))
   #tune.out

 }

naive_bayes <- function()
{
  sessions <- prepare_data()
  x <- sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")]
  y <- sessions[,"markedcategory"]

  classifier <- naiveBayes(x, y) 
  ypred <- predict(classifier, x, type = "class")
  print(table(y, ypred, dnn = list('actual', 'predicted')))
}

prepare_data_for_stacking <- function()
{
  set.seed(1)
  sessions <- prepare_data()
  k <- 10

  pred_by_algos <-  data.frame()
   
  #x <- sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")]
  x <- sessions[,!(names(sessions) %in% c("id", "markedcategory"))]
  y <- sessions[,"markedcategory"]
  x$fold_id <- ceiling(runif(nrow(x), 0.000001, k))

  
  for (i in 1:k)
  { 
    #In iteration i, the ones with fold_id == i form the training set, and the remaining ones form the validation set. 
    #All the learners are learnt from the training set, and applied on the validation set, and the results from the different 
    #classification algorithms for each validation point are kept stored.
    train <- which(x$fold_id != i)
    validation <- which(x$fold_id == i)
    cat(paste("i = ", i, ", length(train) = ", length(train), ", length(validation) = ", length(validation), "\n", sep = ""))

    if (FALSE)
    {
     sessions.rf <- randomForest(x[train, ], y[train], mtry = 1, ntree = 500, prox = TRUE)
     predicted <-  predict(sessions.rf, newdata = x[validation, ])
     pred_by_algos[validation, "randomForest_class"] <- predicted
    }

    tab <- table(y[train])
    bot_class_weight <- as.numeric(tab["User"]/tab["Bot"])
    sessions.svm <- svm(x[train, ], y[train], kernel = "radial", 
                        class.weights = c(Bot = bot_class_weight), 
                        cost = 1000, gamma = 2)
    predicted <-  predict(sessions.svm, newdata = x[validation, ])
    pred_by_algos[validation, "svm_class"] <- predicted

    sessions.rpart <- rpart(markedcategory ~ duration_seconds + n_pages_from_session + n_distinct_pages_from_session, data = sessions[train, ],  
                            minsplit = 10, maxdepth = 5)
    predicted <-  predict(sessions.rpart, newdata = x[validation, ], type = "class")
    pred_by_algos[validation, "rpart_class"] <- predicted

    str_formula <- "markedcategory ~ "
    for (column in colnames(sessions))
    {
     if (column != 'id' & column != 'markedcategory')
     {
       str_formula <- paste(str_formula, column, " + ", sep = "")
     }
    }
    str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
    sessions.nnet <- nnet(as.formula(str_formula), data = sessions[train, ], size = 10)
   
    predicted <- predict(sessions.nnet, newdata = sessions[validation, ], type = "class")
    pred_by_algos[validation, "nnet_class"] <- predicted
  }
  pred_by_algos$true_class <- sessions[,"markedcategory"]
  #print(pred_by_algos[1:5, ])
  write.csv(pred_by_algos, "/Users/blahiri/hiddenmarkovmodel/documents/pred_by_algos_behav_and_ngram_features.csv")
  pred_by_algos
}

apply_stacking_by_rpart <- function()
{
  pred_by_algos <- read.csv("/Users/blahiri/hiddenmarkovmodel/documents/pred_by_algos_behav_and_ngram_features.csv")
  set.seed(1)
  #x <- pred_by_algos[, c("randomForest_class", "svm_class", "rpart_class", "nnet_class")]
  x <- pred_by_algos[, c("svm_class", "rpart_class", "nnet_class")]
  y <- pred_by_algos[,"true_class"]

  for (column in colnames(x))
  {
    x[, column] <- as.factor(x[, column])
  }
  y <- as.factor(y)
  
  train = sample(1:nrow(x), 0.5*nrow(x))
  test = (-train)
  y.test = y[test]
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(x) - length(train)), "\n", sep = ""))
  tab <- table(y[train])
  bot_class_weight <- as.numeric(tab["User"]/tab["Bot"])
  cat(paste("bot_class_weight = ", bot_class_weight, "\n", sep = ""))
 
  tune.out = tune.rpart(true_class ~ 
                                      #randomForest_class + 
                        svm_class + rpart_class, 
                        data = pred_by_algos[train, ], minsplit = c(5, 10, 15), maxdepth = c(1, 3, 5, 7))
  bestmod <- tune.out$best.model

  ypred = predict(bestmod, x[train, ], type = "class")
  cat("Confusion matrix for training data\n")
  print(table(y[train], ypred, dnn = list('actual', 'predicted')))

  cat("Confusion matrix for test data\n")
  ypred = predict(bestmod, x[test, ], type = "class")
  print(table(y.test, ypred, dnn = list('actual', 'predicted')))
  #false_negatives <- y.test[y.test == 'Bot' & ypred == 'User']
  pred_by_algos[test, "predicted"] <- ypred
  #tune.out
  false_negatives <- subset(pred_by_algos, (true_class == 'Bot' & predicted == 'User'))
}

analyze_data_stacking <- function()
{
  #How many of the sessions have got 2 or more algorithms wrong?
  pred_by_algos <- read.csv("/Users/blahiri/hiddenmarkovmodel/documents/pred_by_algos.csv")
  n_sessions <- nrow(pred_by_algos)
  algos <- c("randomForest_class", "svm_class", "rpart_class")
  n_algos <- length(algos)
  hard_sessions <- c()

  for (i in 1:n_sessions)
  {
    n_failing_algos <- 0
    for (j in 1:n_algos)
    {
      if (pred_by_algos[i, algos[j]] != pred_by_algos[i, "true_class"])
      {
        n_failing_algos <- n_failing_algos + 1
      }
    }
    if (n_failing_algos >= 2)
    {
        cat(paste("i = ", i, ", n_failing_algos = ", n_failing_algos, "\n", sep = ""))
        hard_sessions <- append(hard_sessions, i)
    }
  }
  sessions <- prepare_data()
  #The hard sessions show behavior which are not typical for their classes. User sessions are long, bot sessions are short, or
  #bot sessions have few pages, or user sessions have large number of pages.
  print(sessions[hard_sessions, ])
  hard_sessions
}

pca <- function()
{
  sessions <- prepare_data() 
  sessions_for_pca <- sessions[,!(names(sessions) %in% c("id", "markedcategory"))]
  pc <- prcomp(sessions_for_pca, scale = TRUE)
  
  png("./figures/sessions_biplot.png",  width = 1200, height = 960, units = "px")
  biplot(pc, col = c("blue", "red"))
  dev.off()
  
  #Print the coefficients in the first principal component in decreasing order. n_pages_from_session, n_distinct_pages_from_session and duration_seconds rank pretty low, actually.
  #print(sort(pc$rotation[, "PC1"], decreasing = TRUE))
  proj_along_first_two_pcs <- cbind(data.frame(pc$x[, c("PC1", "PC2")]), markedcategory = sessions$markedcategory)
  print(table(proj_along_first_two_pcs$markedcategory))  
  png("./figures/sessions_first_two_pc.png",  width = 1200, height = 960, units = "px")
  p <- ggplot(proj_along_first_two_pcs, aes(x = PC1, y = PC2)) + aes(shape = markedcategory) + geom_point(aes(colour = markedcategory), size = 2) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Projections along first two PCs for user and bot sessions")
  print(p)
  dev.off()
  
  #These are all bots: on 2-PC plot, all user sessions are concentrated in a very small region, but some bot sessions are scattered all over
  anomalous <- subset(proj_along_first_two_pcs, (PC1 >= 0 & PC2 >= 0.25))
  anom_sessions <- sessions[rownames(anom), ]
  #Are these the ones that are being hard to classify?
}





