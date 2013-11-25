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
  return(list("emission_bot" = emission_bot, "emission_user" = emission_user))
}

get_data_single_client <- function(client_ip)
{
  #Get the HTTP requests (observed states) for the IP with max sessions
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
optimal_filtering <- function(emission_bot, emission_user)
{
  cat(paste("nrow(emission_bot) = ", nrow(emission_bot), "\n"))
  cat(paste("nrow(emission_user) = ", nrow(emission_user), "\n"))
  
  #req_seq <- get_data_single_client('192.168.50.219')
  req_seq <- get_data_all_clients()

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
  req_seq <- optimal_filtering(probabilities[["emission_bot"]], probabilities[["emission_user"]])
  visualize_hidden_state_probabilities(req_seq)
  return(req_seq)
}
