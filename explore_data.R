library(randomForest)

explore_data <- function()
{
  #1134 rows; 236 bot sessions, 633 user sessions, 265 not known. Median number of URLs visited in a session is 52.5, median number of unique URLs visited in a session is 32.
  ubs <- read.csv("/Users/blahiri/cleartrail_ddos/data/WebServerDDoSDataSet/UserBrowsingsSession_MarkedSessions.csv", header = TRUE)
  bot_sessions <- subset(ubs, MarkedCategory == 'Bot')
  summary(bot_sessions$TotalURLs)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #  5.0    20.0    54.0   330.0   263.8  5961.0 
  user_sessions <- subset(ubs, MarkedCategory == 'User')
  summary(user_sessions$TotalURLs)
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #5.00   28.00   58.00   80.85   98.00 1673.00

  #User sessions have more unique URLs than bot sessions, on average
  summary(bot_sessions$UniqueURLs)
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #1.00    4.00   16.50   76.31   44.00 2901.00 
  summary(user_sessions$UniqueURLs)
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #3.00   22.00   45.00   57.58   72.00  453.00 
}

prepare_data <- function()
{
  ubs <- read.csv("/Users/blahiri/cleartrail_ddos/data/WebServerDDoSDataSet/UserBrowsingsSession_MarkedSessions.csv", header = TRUE)
  ubs <- subset(ubs, (MarkedCategory == 'User') | (MarkedCategory == 'Bot'))
  cat(paste("nrow(ubs) = ", nrow(ubs), "\n", sep = ""))
  print(table(ubs$MarkedCategory))
  #weights <- ifelse(ubs$change_type == 'increased', 6, 1)
  ubs
}

classify_sessions <- function()
{
  ubs <- prepare_data()
  random_forest(ubs) 
}

logistic_regression <- function(ubs)
{
  ubs.logr <- glm(factor(MarkedCategory) ~ BrowsingSessionDuration + UniqueURLs + TotalURLs + AvgUrlVisited + TotalPages + SDHttpReqIATSec + 
                                            EntropyHttpReqIAT + EntropyContentLength + GetReqCount + RefererURLCount + AvgHttpReqLen + 
                                            EntropyHttpReqLen + SDUrlDepth + ImagePercent + PercentConsecutiveUrls + AvgQueryParameters + 
                                            OKResCount + NotModifiedResCount + AvgPageLoadingTimeSec + AvgPageViewingTimeSec + SDPageViewingTimeSec + 
                                            RequestRatePerSec + TwoXXSuccessResCount + ThreeXXRedirection + FourXXClientErrorResCount + FiveXXServerErrorResCount,  
                   family = binomial("logit"), data = ubs 
                   #weights = weights
                 )
  ubs$predicted_prob <- predict(ubs.logr, newdata = ubs, type = "response")
  print(contrasts(ubs$MarkedCategory))
  #ubs$predicted_category <- ifelse(df_cac$predicted_prob_increase >= 0.5, 'increased', 'did_not_increase')
  #print(summary(cac.logr))
  #print(table(df_cac[,"change_type"], df_cac[, "predicted_change_type"], dnn = list('actual', 'predicted')))
}

random_forest <- function(ubs)
{
  ubs[is.na(ubs)] <- 0
  ubs.rf <- randomForest(ubs[, c("BrowsingSessionDuration", "UniqueURLs", "TotalURLs", "AvgUrlVisited", "TotalPages", "SDHttpReqIATSec", 
                                "EntropyHttpReqIAT", "EntropyContentLength", "GetReqCount", "RefererURLCount", "AvgHttpReqLen",  
                                "EntropyHttpReqLen", "SDUrlDepth", "ImagePercent", "PercentConsecutiveUrls", "AvgQueryParameters",
                                "OKResCount", "NotModifiedResCount", "AvgPageLoadingTimeSec", "AvgPageViewingTimeSec", "SDPageViewingTimeSec", 
                                "RequestRatePerSec", "TwoXXSuccessResCount", "ThreeXXRedirection", "FourXXClientErrorResCount", "FiveXXServerErrorResCount")], 
                         ubs[,"MarkedCategory"], 
                         prox = TRUE)

  ubs$predicted <-  ubs.rf$predicted
  print(table(ubs[,"MarkedCategory"], ubs[, "predicted"], dnn = list('actual', 'predicted')))
}
