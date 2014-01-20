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

train_validate_test_nn <- function()
 {
   set.seed(1)
   sessions <- prepare_data()
   x <- sessions[, c("duration_seconds", "n_pages_from_session", "n_distinct_pages_from_session")]
   y <- sessions[,"markedcategory"]
   train = sample(1:nrow(x), 0.5*nrow(x))
   test = (-train)
   y.test = y[test]
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(sessions) - length(train)), "\n", sep = ""))

   #bots_ann = nnet(x[train, ], class.ind(y[train]), size = 2)
   bots_ann <- nnet(markedcategory ~ duration_seconds + n_pages_from_session + n_distinct_pages_from_session, data = sessions[train, ], size = 2) 
   ypred <- predict(bots_ann, x[test, ], type = "class")
   
   #bestmod <- tune.out$best.model
   #ypred <- predict(bestmod, newdata = sessions[test, ], type = "class")
   print(ypred)
   print(table(y.test, ypred, dnn = list('actual', 'predicted')))
   #tune.out

 }



