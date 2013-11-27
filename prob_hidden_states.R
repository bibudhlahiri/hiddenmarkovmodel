library(RPostgreSQL)
init_probs <- c(0.73, 0.27)

trans_probs <- matrix(c(0.88, 0.12, 0.31, 0.69), nrow = 2, byrow = TRUE)
rownames(trans_probs) <- c('User', 'Bot')
colnames(trans_probs) <- c('User', 'Bot')

compute_emission_probs <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
  statement <- paste("select distinct hr.urlids as urlid
                       from interesting_sessions i, http_requests hr
                       where i.ClientIPServerIP = hr.ClientIPServerIP
                       and i.browsingsessionid = hr.browsingsessionid
                       order by hr.urlids", sep = "")
  res <- dbSendQuery(con, statement);
  interesting_urls <- fetch(res, n = -1) 

  statement <- paste("select hr.urlids as urlid, cast(count(*) as real)/72313 as probability
                         from interesting_sessions i, http_requests hr
                         where i.ClientIPServerIP = hr.ClientIPServerIP
                         and i.browsingsessionid = hr.browsingsessionid
                         and i.MarkedCategory = 'Bot'
                         group by hr.urlids
                         order by count(*) desc", sep = "")
  res <- dbSendQuery(con, statement);
  emission_bot <- fetch(res, n = -1)

  emission_bot <- merge(x = interesting_urls, y = emission_bot, all.x = TRUE, by.x = "urlid", by.y = "urlid")
  emission_bot[is.na(emission_bot)] <- 0
  

  statement <- paste("select hr.urlids as urlid, cast(count(*) as real)/46857 as probability
                      from interesting_sessions i, http_requests hr
                      where i.ClientIPServerIP = hr.ClientIPServerIP
                      and i.browsingsessionid = hr.browsingsessionid
                      and i.MarkedCategory = 'User'
                      group by hr.urlids
                      order by count(*) desc", sep = "")
  res <- dbSendQuery(con, statement);
  emission_user <- fetch(res, n = -1)

  emission_user <- merge(x = interesting_urls, y = emission_user, all.x = TRUE, by.x = "urlid", by.y = "urlid")
  emission_user[is.na(emission_user)] <- 0
  dbDisconnect(con) 
  
  cat(paste("nrow(emission_bot) = ", nrow(emission_bot), "\n"))
  cat(paste("nrow(emission_user) = ", nrow(emission_user), "\n"))

  return(list("emission_bot" = emission_bot, "emission_user" = emission_user))
}

get_data_single_client <- function(client_ip)
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
  statement <- paste("select i.browsingsessionid, i.MarkedCategory, hr.urlids as urlid
                      from interesting_sessions i, http_requests hr
                      where i.ClientIPServerIP = hr.ClientIPServerIP
                      and i.browsingsessionid = hr.browsingsessionid
                      and client_ip = '", client_ip, "'", 
                      " order by i.browsingsessionid, hr.timestamp", sep = "")
  res <- dbSendQuery(con, statement);
  req_seq <- fetch(res, n = -1)
  dbDisconnect(con)
  return(req_seq)
}

get_data_all_clients <- function()
{
  #Get the HTTP requests (observed states) for all IPs
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
  statement <- paste("select i.client_ip, i.browsingsessionid, i.MarkedCategory, hr.urlids as urlid
                      from interesting_sessions i, http_requests hr
                      where i.ClientIPServerIP = hr.ClientIPServerIP
                      and i.browsingsessionid = hr.browsingsessionid
                      order by i.client_ip, i.browsingsessionid, hr.timestamp", sep = "")
  res <- dbSendQuery(con, statement);
  req_seq <- fetch(res, n = -1)
  dbDisconnect(con)
  return(req_seq)
}


#Compute the values of P(X_t = 'Bot'/Y_{1:(t-1)}), given the initial probabilities, transition probabilities and emission probabilities, 
#using the dynamic programming algorithm discussed by Nando de Freitas
optimal_filtering <- function(emission_bot, emission_user, req_seq)
{
    
  #State prediction (P(X_t/Y_{1:(t-1)})) and Bayesian update (P(X_t/Y_{1:t})) go alternatively, and each 
  #provide input to the other

  #We start by first round of Bayesian update  
  p_x_1_bot <- init_probs[2]
  p_y1_given_x1_bot <- (subset(emission_bot, (emission_bot$urlid == req_seq[1, "urlid"])))$probability

  p_x_1_user <- init_probs[1]
  p_y1_given_x1_user <- (subset(emission_user, (emission_user$urlid == req_seq[1, "urlid"])))$probability
 
  bayes_update_bot <- p_x_1_bot*p_y1_given_x1_bot/(p_x_1_bot*p_y1_given_x1_bot + p_x_1_user*p_y1_given_x1_user)
  bayes_update_user <- p_x_1_user*p_y1_given_x1_user/(p_x_1_bot*p_y1_given_x1_bot + p_x_1_user*p_y1_given_x1_user)

  req_seq[1, "bayes_update_bot"] <- bayes_update_bot
  req_seq[1, "bayes_update_user"] <- bayes_update_user

  T <- nrow(req_seq)
  for (t in 2:T)
  {
    #First do state prediction, then use it for Bayesian update, and repeat that in loop
    #Add the terms for X_{t-1} = 'User' and X_{t-1} = 'Bot'
    state_prediction_bot <- trans_probs["User", "Bot"]*req_seq[(t-1), "bayes_update_user"] + trans_probs["Bot", "Bot"]*req_seq[(t-1), "bayes_update_bot"]
    req_seq[t, "state_prediction_bot"] <- state_prediction_bot

    state_prediction_user <- trans_probs["User", "User"]*req_seq[(t-1), "bayes_update_user"] + trans_probs["Bot", "User"]*req_seq[(t-1), "bayes_update_bot"]
    req_seq[t, "state_prediction_user"] <- state_prediction_user

    p_yt_given_xt_bot <- (subset(emission_bot, (emission_bot$urlid == req_seq[t, "urlid"])))$probability
    p_yt_given_xt_user <- (subset(emission_user, (emission_user$urlid == req_seq[t, "urlid"])))$probability
    bayes_update_bot <- p_yt_given_xt_bot*req_seq[t, "state_prediction_bot"]/(p_yt_given_xt_bot*req_seq[t, "state_prediction_bot"] + p_yt_given_xt_user*req_seq[t, "state_prediction_user"])
    bayes_update_user <- p_yt_given_xt_user*req_seq[t, "state_prediction_user"]/(p_yt_given_xt_bot*req_seq[t, "state_prediction_bot"] + p_yt_given_xt_user*req_seq[t, "state_prediction_user"])
    req_seq[t, "bayes_update_bot"] <- bayes_update_bot
    req_seq[t, "bayes_update_user"] <- bayes_update_user
  }
  #known_bot_sessions <- subset(req_seq, (markedcategory == 'Bot'))
  #print(known_bot_sessions)
   
  return(req_seq)
}

#Taking the sessions and HTTP requests of client_ip = 192.168.50.219 only, the number of sessions where estimated probability of a bot session ever reached 0.5 was 42. 
#This is 49% of total sessions (86). The one bot session does raise an alarm. 
visualize_hidden_state_probabilities <- function(req_seq)
{
  req_seq <- req_seq[, c("markedcategory", "bayes_update_bot")]
  req_seq$sequence_no <- 1:nrow(req_seq)
  png(file = "./figures/filtering_probabilities.png", width = 1000, height = 600)
  p <- ggplot(req_seq, aes(x = sequence_no, y = bayes_update_bot, fill = markedcategory)) + geom_bar(stat="identity", binwidth = 1) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.text.x = element_text(angle = 90)) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  dev.off() 
}

call_all <- function()
{
  probabilities <- compute_emission_probs()
  #192.168.50.93 has 13 bot sessions and 14 user sessions: the one with most even distribution of bot and user sessions
  #192.168.50.219 has max number of total sessions
  req_seq <- get_data_single_client('192.168.50.93')
  #req_seq <- get_data_all_clients()
  req_seq <- optimal_filtering(probabilities[["emission_bot"]], probabilities[["emission_user"]], req_seq)
  #visualize_hidden_state_probabilities(req_seq)
  return(req_seq)
}

measure_perf_all_IPs <- function()
{ 
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cleartrail")
  statement <- paste("select client_ip, 
                      count(distinct browsingsessionid) n_sessions, 
                      sum(case when markedcategory = 'User' then 1 else 0 end) as n_user_sessions,
                      sum(case when markedcategory = 'Bot' then 1 else 0 end) as n_bot_sessions
                      from interesting_sessions
                      --where client_ip = '192.168.50.93' 
                      group by client_ip
                      having count(distinct browsingsessionid) >= 10
                      order by count(distinct browsingsessionid) desc", sep = "")
  res <- dbSendQuery(con, statement);
  major_ips <- fetch(res, n = -1)
  n_major_ips <- nrow(major_ips)
  threshold_prob <- 0.5
  probabilities <- compute_emission_probs()
  for (i in 1:n_major_ips)
  {
    cat(paste("Processing IP ", major_ips[i, "client_ip"], "\n", sep = "")) 
    req_seq <- get_data_single_client(major_ips[i, "client_ip"])
    req_seq <- optimal_filtering(probabilities[["emission_bot"]], probabilities[["emission_user"]], req_seq)
    req_seq$raise_alarm <- as.numeric(req_seq$bayes_update_bot >= threshold_prob)

    session_alarms <- aggregate(x = req_seq$raise_alarm, by = list(req_seq$browsingsessionid), FUN = sum, na.rm = TRUE) 
    colnames(session_alarms) <- c("browsingsessionid", "raise_alarm")
    session_alarms$raise_alarm <- as.numeric(session_alarms$raise_alarm > 0)

    session_labels <- unique(req_seq[, c('browsingsessionid', 'markedcategory')])
    session_labels_alarms <- merge(x = session_labels, y = session_alarms)

    session_labels_alarms$false_positives <- as.numeric((session_labels_alarms$markedcategory == 'User') & (session_labels_alarms$raise_alarm == 1))
    session_labels_alarms$false_negatives <- as.numeric((session_labels_alarms$markedcategory == 'Bot') & (session_labels_alarms$raise_alarm == 0))

    false_negatives <- sum(session_labels_alarms$false_negatives)
    false_positives <- sum(session_labels_alarms$false_positives)
    fpr <- 0
    if (major_ips[i, "n_user_sessions"] > 0)
    {
      fpr <- false_positives/major_ips[i, "n_user_sessions"]
    }
    fnr <- 0
    if (major_ips[i, "n_bot_sessions"] > 0)
    {
      fnr <- false_negatives/major_ips[i, "n_bot_sessions"]
    }
    cat(paste("n_sessions = ", major_ips[i, "n_sessions"], ", n_user_sessions = ", major_ips[i, "n_user_sessions"], 
              ", n_bot_sessions = ", major_ips[i, "n_bot_sessions"], ", fpr = ", fpr, ", fnr = ", fnr, "\n", sep = ""))
    major_ips[i, "fpr"] <- fpr
    major_ips[i, "fnr"] <- fnr
  }
  dbDisconnect(con)
  return(major_ips)
} 
