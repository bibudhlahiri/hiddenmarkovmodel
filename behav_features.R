library(randomForest)
library(RPostgreSQL)
library(e1071)
library(nnet)

prepare_data <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
  statement <- "select bs.ClientIPServerIP, bs.browsingsessionid, bs.MarkedCategory, max(hr.timestamp) - min(hr.timestamp) as duration_seconds
                from browsing_sessions bs, http_requests hr
                where bs.ClientIPServerIP = hr.ClientIPServerIP
                and bs.browsingsessionid = hr.browsingsessionid
                and bs.MarkedCategory in ('User', 'Bot')
                group by bs.ClientIPServerIP, bs.browsingsessionid, bs.MarkedCategory"
  res <- dbSendQuery(con, statement)
  sess_durations <- fetch(res, n = -1) 

  statement <- "select bs.ClientIPServerIP, bs.browsingsessionid, count(*) n_pages_from_session
                from browsing_sessions bs, http_requests_pages hr
                where bs.ClientIPServerIP = hr.ClientIPServerIP
                and bs.browsingsessionid = hr.browsingsessionid
                and bs.MarkedCategory in ('User', 'Bot')
                group by bs.ClientIPServerIP, bs.browsingsessionid"
  res <- dbSendQuery(con, statement)
  sess_pages <- fetch(res, n = -1)
  

  sessions <- merge(sess_durations, sess_pages, all.x = TRUE) 


  statement <- "select bs.ClientIPServerIP, bs.browsingsessionid, count(distinct hr.completeurl) n_distinct_pages_from_session
                from browsing_sessions bs, http_requests_pages hr
                where bs.ClientIPServerIP = hr.ClientIPServerIP
                and bs.browsingsessionid = hr.browsingsessionid
                and bs.MarkedCategory in ('User', 'Bot')
                group by bs.ClientIPServerIP, bs.browsingsessionid
                order by n_distinct_pages_from_session"
  res <- dbSendQuery(con, statement)
  sess_distinct_pages <- fetch(res, n = -1)

  sessions <- merge(sessions, sess_distinct_pages, all.x = TRUE)
  sessions[is.na(sessions)] <- 0
  #print(sessions)
  sessions[,"markedcategory"] <- as.factor(sessions[,"markedcategory"])
  dbDisconnect(con)
  return(sessions)
}


#Classification with simple attributes like number of pages visited in session, duration of session, etc
classify_rf <- function()
{
  set.seed(1)
  sessions <- prepare_data()
  #Random forest does not need splitting data into training and test sets, or cross-validation, as the prediction on a point 
  #is based on trees (about 1/3rd) where it is out-of-bag. The trees differ in two things: each tree takes a bootstrap sample of 
  #the training data, and picks mtry features at random to build the tree.
  sessions.rf <- randomForest(sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")],  
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

   tune.out = tune.randomForest(markedcategory ~ duration_seconds + n_pages_from_session + n_distinct_pages_from_session, 
                                data = sessions[train, ], ntree = seq(100, 600, 100), mtry = c(1, 2))
   bestmod <- tune.out$best.model
   ypred <- predict(bestmod, newdata = sessions[test, ], type = "class")
   print(table(sessions[test, "markedcategory"], ypred, dnn = list('actual', 'predicted')))
   tune.out
 }


train_validate_test_svm <- function()
{
  set.seed(1)
  sessions <- prepare_data()
  x <- sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")]
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
  #tune.out = tune.svm(x[train, ], y[train], kernel = "polynomial", cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4))
  bestmod <- tune.out$best.model

  ypred = predict(bestmod, x[train, ])
  cat("Confusion matrix for training data\n")
  print(table(y[train], ypred, dnn = list('actual', 'predicted')))

  cat("Confusion matrix for test data\n")
  ypred = predict(bestmod, x[test, ])
  print(table(y.test, ypred, dnn = list('actual', 'predicted')))
  tune.out
 }

 train_validate_test_knn <- function()
 {
   set.seed(1)
   sessions <- prepare_data()
   x <- sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")]
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

train_validate_test_rpart <- function()
 {
   set.seed(1)
   sessions <- prepare_data()
   train = sample(1:nrow(sessions), 0.5*nrow(sessions))
   test = (-train)
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(sessions) - length(train)), "\n", sep = ""))

   tune.out = tune.rpart(markedcategory ~ duration_seconds + n_pages_from_session + n_distinct_pages_from_session, data = sessions[train, ], minsplit = c(5, 10, 15), maxdepth = c(1, 3, 5, 7))
   bestmod <- tune.out$best.model
   ypred <- predict(bestmod, newdata = sessions[test, ], type = "class")
   print(table(sessions[test, "markedcategory"], ypred, dnn = list('actual', 'predicted')))
   tune.out
 }

train_validate_test_nn_single_hidden_layer <- function()
 {
   set.seed(1)
   sessions <- prepare_data()
   x <- sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")]
   y <- sessions[,"markedcategory"]
   train = sample(1:nrow(x), 0.5*nrow(x))
   test = (-train)
   y.test = y[test]
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(sessions) - length(train)), "\n", sep = ""))

   tune.out = tune.nnet(markedcategory ~ duration_seconds + n_pages_from_session + n_distinct_pages_from_session, data = sessions[train, ], size = seq(2, 12, 2)) 
   
   bestmod <- tune.out$best.model
   ypred <- predict(bestmod, newdata = sessions[test, ], type = "class")
   print(table(y.test, ypred, dnn = list('actual', 'predicted')))
   tune.out
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

prepare_data_for_stacking <- function()
{
  set.seed(1)
  sessions <- prepare_data()
  k <- 10
  
  algos <- c("randomForest", "svm", "rpart", "true")
  cat(paste("length(algos) = ", length(algos), "\n", sep = ""))

  pred_by_algos <-  data.frame()
   
  x <- sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")]
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
    sessions.rf <- randomForest(x[train, ],  
                                y[train], mtry = 1, ntree = 500,
                                prox = TRUE)
    predicted <-  predict(sessions.rf, newdata = x[validation, ])
    pred_by_algos[validation, "randomForest"] <- predicted

    tab <- table(y[train])
    bot_class_weight <- as.numeric(tab["User"]/tab["Bot"])
    sessions.svm <- svm(x[train, ], y[train], kernel = "radial", 
                        class.weights = c(Bot = bot_class_weight), 
                        cost = 100, gamma = 4)
    predicted <-  predict(sessions.svm, newdata = x[validation, ])
    pred_by_algos[validation, "svm"] <- predicted

    sessions.rpart <- rpart(markedcategory ~ duration_seconds + n_pages_from_session + n_distinct_pages_from_session, data = sessions[train, ],  
                            minsplit = 10)
    predicted <-  predict(sessions.rpart, newdata = x[validation, ], type = "class")
    pred_by_algos[validation, "rpart"] <- predicted
  }
  pred_by_algos$true <- sessions[,"markedcategory"]
  print(pred_by_algos[1:5, ])
  pred_by_algos
} 



