library(randomForest)
library(RPostgreSQL)

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
