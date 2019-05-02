###################################################################################################
# Combine and process streamed data and make training set
###################################################################################################
# Description
###################################################################################################
# This script contains A all unctions and steps to prepare the training data for the deep learning 
# network and B it contains all the setps to build the giant dataset of all the weets collected for 
# classification
###################################################################################################
# Content
###################################################################################################
# 1) Dependencies
# 2) Preparations
# 3) Functions 
## 3.1) Processing functions for training data from Cresci et al.2017
## 3.2) Processing functions for collected data (Midterm-Stream)
# 4) Process training data
## 4.1) Load & combine trainging data 
## 4.2) Process the training data
## 4.3) Save Data
# 5) Process collected data
## 5.1) Get path of all JSON  
## 5.2) Process Collected data
## 5.3) Combine all Days into one single Data Frame
## 5.4) Make data frame for predictions by user and data frame by tweets with descriptive satistics
### 5.4.1) Calculate Descriptives
### 5.4.2) Save data on the tweet level
### 5.4.3) Save data on the user level
###################################################################################################
# 1) Dependencies
###################################################################################################
library(readr)
library(data.table)
library(stringi)
library(stringr)
library(bit64)
library(dplyr)
library(car)
library(sentimentr)
library(microbenchmark)
library(parallel)
library(doParallel)
library(foreach)
library(tcltk)
library(iterators)
library(textclean)
library(stargazer)
library(Hmisc)
library(rtweet)
###################################################################################################
# 1) Preparations
###################################################################################################
rm(list=ls(all=T)) # cleans workspace
options(stringsAsFactors = F) # gets rid of factors
###################################################################################################
# 3) Functions
###################################################################################################
## 3.1) Processing functions for training data from Cresci et al.2017
###################################################################################################
# First processing steps indlucdes nothing more than droping unused features and renaming features 
# for coherent names with collected data
prepfuntraindata1 <- function(data){
  data$status_id <- data$id
  data$created_at <- data$timestamp.x
  data$reply_to_screen_name <- data$in_reply_to_screen_name
  data$is_retweet <- ifelse(data$retweeted_status_id == 0, FALSE,TRUE)
  data$n_hashtags <- data$num_hashtags
  data$n_url <- data$num_urls
  data$n_mentions <- data$num_mentions
  data$location <- data$location
  data$account_created_at <- data$timestamp.y
  data$account_crawled_at <- data$crawled_at.y
  data$account_lang <- data$lang
  data$profile_background_url <- data$profile_background_image_url_https
  data$profile_banner_url <- data$profile_banner_url
  data$profile_image_url <- data$profile_image_url_https
  data$reply_to_screen_name <- data$in_reply_to_screen_name
  
  cnames <- c("id", "in_reply_to_status_id", "in_reply_to_user_id", "favorite_count", "retweet_count", "truncated", "geo",
              "place", "contributors", "reply_count", "favorited", "retweeted", "possibly_sensitive", "num_hashtags",
              "num_urls", "num_mentions", "created_at.x", "timestamp.x", "crawled_at.x", "updated.x", "time_zone",
              "is_translator", "follow_request_sent", "protected", "notifications", "contributors_enabled", "in_reply_to_screen_name",
              "created_at.y", "timestamp.y", "crawled_at.y", "updated.y", "profile_text_color", "profile_sidebar_border_color",
              "profile_background_tile", "profile_sidebar_fill_color", "profile_background_color", "utc_offset", "profile_use_background_image")
  data <- data %>% select(-one_of(cnames)) 
  
  return(data)
}

# Second porcessing steps including mostly simple transformations of features to numeric values for DNN
prepfuntraindata2 <- function(data){
  # Convert Important Variables to Boolean 
  data$status_id <- as.numeric(data$status_id)                                    #Make Status ID numeric
  data$user_id <- as.numeric(data$user_id)                                        #Make User ID numeric
  data$is_retweet <- ifelse(data$is_retweet == TRUE, 1, 0)                        #Retweet --> BOOLEAN
  data$is_reply <- ifelse(is.na(data$reply_to_screen_name), 0,1)                  #Reply --> BOOLEAN
  data$verified <- ifelse(data$verified == TRUE, 1, 0)                            #Verified --> BOOLEAN
  data$media_url <- ifelse(data$n_url >= 1, 1, 0)                                 #URL Shared in Tweet --> BOOLEAN
  data$profile_banner_default <-   ifelse(data$profile_banner_url == "NULL", 1,0) #Profile Banner --> BOOLEAN
  data$profile_background_default <- ifelse(data$profile_background_url == "https://abs.twimg.com/images/themes/theme1/bg.png", 1,0) #Profile Default Background Theme
  data$profile_image_default <- ifelse(is.na(data$default_profile_image), 0, 1)   #Default Profile Picture --> BOOLEAN
  data$screen_name_digits <- nchar(gsub("\\D", "", data$screen_name))             #Number of Digits in Screen Name --> NUMERIC
  data$screen_name_length <- nchar(data$screen_name)                              #Number of Characters in Screen Name --> NUMERIC
  data$name_digits <- nchar(gsub("\\D", "", data$name))                           #Number of Digits in Name --> NUMERIC
  data$name_length <- nchar(data$name)                                            #Number of Characters in Name --> NUMERIC
  data$description_digits <- nchar(gsub("\\D", "", data$description))             #Number of Digits in Description --> NUMERIC
  data$description_digits[is.na(data$description_digits)] <- 0                    #Number of Digits in Description --> NUMERIC (replace NA with 0)
  data$description_length <- nchar(data$description)                              #Number of Characters in Description --> Numeric
  data$description_length[is.na(data$description_length)] <- 0                    #Number of Characters in Description --> NUMERIC (replace NA with 0)
  data$geo_enabled[is.na(data$geo_enabled)] <- 0
  data$default_profile[is.na(data$default_profile)] <- 0
  data$default_profile_image[is.na(data$default_profile_image)] <- 0
  
  data$reply_to_screen_name               <- NULL
  data$in_reply_to_screen_name            <- NULL
  data$n_url                              <- NULL
  data$profile_background_url             <- NULL
  data$profile_background_image_url_https <- NULL
  data$profile_background_image_url       <- NULL
  data$profile_banner_url                 <- NULL
  
  return(data)
}

# Third processing steps including complex things like sentiment and hashtag extraction...
prepfuntraindata3 <- function(data, nodes = 1){ 
  # Get Sentiment NUMERIC / Hashtags LIST / Mentions LIST  / Word Count of Text NUMERIC (Parallel)
  clu <- makeCluster(nodes)
  registerDoParallel(clu)
  
  numoit <- nrow(data)
  resi <- data.frame(matrix(NA, ncol = 4, nrow = numoit))
  colnames(resi) <- c("sentiment", "hashtags", "mentions_screen_name", "word_count")
  #pb <- txtProgressBar(max=numoit, style = 3)
  #progress <- function(n) setTxtProgressBar(pb,n)
  #opts <- list(progress = progress)
  
  results <- foreach(i = 1:numoit, .combine = "rbind", .packages = c("sentimentr", "stringr", "stringi", "dplyr", "tcltk")) %dopar% {
    if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=numoit)
    setTkProgressBar(pb, i)
    text <- data[i,'text']
    textall <- paste(data[i,'text'])
    hash <- str_extract_all(textall, "#\\S+")
    ment <- str_extract_all(textall, "@\\S+")
    text = gsub("&amp", "", text)
    text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
    text = gsub("@\\w+", "", text)
    text = gsub("[[:punct:]]", "", text)
    text = gsub("[[:digit:]]", "", text)
    text = gsub("http\\w+", "", text)
    text = gsub("[ \t]{2,}", "", text)
    text = gsub("^\\s+|\\s+$", "", text) 
    word_count <- sapply(strsplit(text, " "), length)
    sent <- sentiment(text)
    meansent <- mean(sent$sentiment)
    sentiment <- round(meansent, digits = 4)
    hashtags <- ifelse(list(hash[[1]]) == "character(0)", " ", list(hash[[1]]))   #Make list of all mentioned Hashtags
    ment <- ifelse(list(ment[[1]]) == "character(0)", " ", list(ment[[1]]))       #Make ist of all mentioned Uers
    mentions_screen_name <- ment[[1]]
    
    resi[i,'sentiment'] <- sentiment
    resi$hashtags[i] <- list(hashtags[[1]])                         #Format df$var[row_n] due to list input
    resi$mentions_screen_name[i] <- list(mentions_screen_name[[1]]) #Format df$var[row_n] due to list input
    resi[i, 'word_count'] <- word_count
    resi[i,]
    
  }
  
  on.exit(parallel::stopCluster(clu))
  #on.exit(close(pb))
  
  data$sentiment <- results[,1]
  data$hashtags <- results[,2]                #Make list of all mentioned Hashtags                
  data$mentions_screen_name <- results[,3]    #Make ist of all mentioned Uers
  data$word_count <- results[,4]
  
  
  #Mentions Names character(0) to " "
  data$mentions_screen_name[data$mentions_screen_name == "character(0)"] <- " "
  
  #Age of Account NUMERIC (days) & Get number of Hashtags / Mentions (Parallel)
  clu <- makeCluster(nodes)
  registerDoParallel(clu)
  
  numoit <- nrow(data)
  res <- data.frame(matrix(NA, ncol = 1, nrow = numoit))
  colnames(res) <- c("account_age")
  
  result <- foreach(i = 1:numoit, .combine = "rbind", .packages = c("tcltk")) %dopar% {
    if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=numoit)
    setTkProgressBar(pb, i)
    
    data_crea <- data[i, 'account_created_at']
    data_craw <- data[i, 'account_crawled_at']
    
    data_crea <- as.Date(data_crea[[1]], format = "%Y-%m-%d")
    data_craw <- as.Date(data_craw[[1]], format = "%Y-%m-%d")
    
    account_age <- as.numeric(data_craw - data_crea)
    res[i,] <- account_age
  }
  
  on.exit(parallel::stopCluster(clu))
  
  data$account_age <- result[,1]
  
  #Remove all Columns which have been used and are no longer important since they are made new as 
  #numerics and are no longer used for the later analysis:
  data$description                        <- NULL
  data$source                             <- NULL
  data$profile_image_url                  <- NULL
  data$profile_image_url_https            <- NULL
  data$text                               <- NULL
  data$profile_link_color                 <- NULL
  data$url                                <- NULL
  data$following                          <- NULL
  data$retweeted_status_id                <- NULL
  
  data <- data[c("Bot","user_id","status_id", "created_at", "screen_name", "is_retweet", "hashtags", 
                 "mentions_screen_name", "lang", "name", "location", "followers_count", 
                 "friends_count", "listed_count", "statuses_count", "favourites_count", "account_created_at", "account_crawled_at",
                 "verified", "account_lang", "is_reply", "media_url", "profile_banner_default", "profile_background_default",
                 "profile_image_default", "screen_name_digits", "screen_name_length", "name_digits", "name_length", 
                 "description_digits", "description_length", "geo_enabled", 
                 "sentiment", "word_count", "account_age", "n_hashtags", "n_mentions")]
  
  data$created_at <- NULL
  data$account_crawled_at <- NULL
  data$account_created_at <- NULL
  data$hashtags <- NULL
  data$mentions_screen_name <- NULL
  
  return(data)
  parallel::stopCluster(clu)
}

# Fourth processing steps including aggregated tweet informations add ratio variables ...
prepfuntraindata4 <- function(data){
  data <- data %>% 
    group_by(user_id) %>% 
    mutate(mean_sentiment = mean(sentiment),
           median_sentiment = median(sentiment),
           sd_sentiment = sd(sentiment),
           mean_word_count = mean(word_count),
           median_word_count = median(word_count),
           sd_word_count = sd(word_count),
           mean_n_hashtags = mean(n_hashtags),
           median_n_hashtags = median(n_hashtags),
           sd_n_hashtags = sd(n_hashtags),
           mean_n_mentions = mean(n_mentions),
           median_n_mentions = median(n_mentions),
           sd_n_mentions = sd(n_mentions),
           retweet_ratio = mean(is_retweet),
           URL_ratio = mean(media_url),
           reply_ratio = mean(is_reply))
  
  # ADD Ratio Variables to training data frame:
  ## Friends Follower Ratio:
  data$friends_follower_ratio <- data$friends_count/data$followers_count 
  ###summary(data$friends_follower_ratio)
  ### Check what values to give NA's:
  ###table <- data %>% filter(friends_follower_ratio == 0 | is.na(friends_follower_ratio)) %>% group_by(Bot, friends_follower_ratio) %>% summarize(n = n())
  ### Both 0 and NA are all Bots! Therefore I change the NA'a to Zero as well! 
  data$friends_follower_ratio[is.na(data$friends_follower_ratio)] <- 0
  
  ## Favourites Statuses Ratio:
  data$favourites_statuses_ratio <- data$favourites_count/data$statuses_count
  ###summary(data$favourites_statuses_ratio)
  ### No action needed!
  
  ## Listed Statuses Ratio:
  data$listed_statuses_ratio <- data$listed_count/data$statuses_count
  ###summary(data$listed_statuses_ratio)
  ### No action needed!
  
  ## Tweets per Day:
  data$tweets_per_day <- data$statuses_count/data$account_age
  ###summary(data$tweets_per_day)
  ### No action needed!
  
  # Verifed Variable to 0 and 1 all NA's are non verified accounts
  data$verified[is.na(data$verified)] <- 0
  
  # Save Data Frame with all variables including the ratio-variables 
  colnames(data) <- iconv(colnames(data), to='ASCII', sub='')
  
  return(data)
}
###################################################################################################
## 3.2) Processing functions for collected data (Midterm-Stream)
###################################################################################################
# First processing steps indlucdes nothing more than droping unused features
prepfunstreamdata1 <- function(data){
  # Remove all columns which are unimportant and not used at all:
  cnames <- c("display_text_width", "reply_to_status_id", "reply_to_user_id", "symbols", "favorite_count", "retweet_count", 
              "urls_url", "urls_t.co", "urls_expanded_url",
              "media_url", "media_t.co", "media_expanded_url","ext_media_url", "ext_media_t.co", "ext_media_type", "media_type",
              "mentions_user_id", "url",
              "quoted_status_id", "quoted_created_at", "quoted_source", "quoted_favorite_count", "quoted_retweet_count", 
              "quoted_screen_name", "quoted_name", "quoted_followers_count", "quoted_friends_count", "quoted_statuses_count", 
              "quoted_location",  "quoted_description", "quoted_verified",
              "retweet_status_id" , "retweet_created_at", "retweet_source", "retweet_favorite_count", "retweet_retweet_count", 
              "retweet_screen_name", "retweet_name", "retweet_followers_count", "retweet_friends_count", "retweet_statuses_count", 
              "retweet_location", "retweet_description", "retweet_verified",
              "place_url", "place_full_name", "place_type", "country",
              "geo_coords", "coords_coords", "bbox_coords", "protected", "profile_url")
  data <- data %>% select(-one_of(cnames)) 
  return(data)
}

# Second porcessing steps including mostly simple transformations of features to numeric values for DNN, sentiment and hashtag extraction...
prepfunstreamdata2 <- function(data, nodes = 1) {
  
  # Convert Important Variables to Boolean 
  data$status_id <- as.numeric(data$status_id)                                #Make Status ID numeric
  data$user_id <- as.numeric(data$user_id)                                    #Make User ID numeric
  data$is_retweet <- ifelse(data$is_retweet == TRUE, 1, 0)                    #Retweet --> BOOLEAN
  data$is_quote <- ifelse(data$is_quote == TRUE, 1,0)                         #Quote --> Boolean
  data$is_retweet <- ifelse(data$is_retweet ==1 | data$is_quote ==1, 1,0)     #Merge Quote and Retweet to Retweet since Cresci's DF does not know Quote which is very similar to Retweet
  data$is_reply <- ifelse(is.na(data$reply_to_screen_name), 0,1)              #Reply --> BOOLEAN
  data$verified <- ifelse(data$verified == TRUE, 1, 0)                        #Verified --> BOOLEAN
  data$media_url <- ifelse(is.na(data$ext_media_expanded_url), 0, 1)          #URL Shared in Tweet --> BOOLEAN
  data$profile_banner_default <- ifelse(is.na(data$profile_banner_url), 0,1)  #Profile Banner --> BOOlEAN
  data$profile_background_default <- ifelse(data$profile_background_url == "", 1,0) #Profile Default Background Theme
  data$profile_image_default <- ifelse(data$profile_image_url == "http://abs.twimg.com/sticky/default_profile_images/default_profile_normal.png", 1, 0) #Default Profile Picture --> BOOLEAN
  data$screen_name_digits <- nchar(gsub("\\D", "", data$screen_name))         #Number of Digits in Screen Name --> NUMERIC
  data$screen_name_length <- nchar(data$screen_name)                          #Number of Characters in Screen Name --> NUMERIC
  data$name_digits <- nchar(gsub("\\D", "", data$name))                       #Number of Digits in Name --> NUMERIC
  data$name_length <- nchar(data$name)                                        #Number of Characters in Name --> NUMERIC
  data$description_digits <- nchar(gsub("\\D", "", data$description))         #Number of Digits in Description --> NUMERIC
  data$description_digits[is.na(data$description_digits)] <- 0                #Number of Digits in Description --> NUMERIC (replace NA with 0)
  data$description_length <- nchar(data$description)                          #Number of Characters in Description --> Numeric
  data$description_length[is.na(data$description_length)] <- 0                ##Number of Characters in Description --> NUMERIC (replace NA with 0)
  data$geo_enabled <- ifelse(!is.na(data$place_name), 1,0)
  
  # Get Sentiment NUMERIC / Hashtags LIST / Mentions LIST  / Word Count of Text NUMERIC (Parallel)
  clu <- makeCluster(nodes)
  registerDoParallel(clu)
  
  numoit <- nrow(data)
  resi <- data.frame(matrix(NA, ncol = 4, nrow = numoit))
  colnames(resi) <- c("sentiment", "hashtags", "mentions_screen_name", "word_count")
  
  results <- foreach(i = 1:numoit, .combine = "rbind", .packages = c("sentimentr", "stringr", "stringi", "dplyr", "tcltk")) %dopar% {
    if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=numoit)
    setTkProgressBar(pb, i)
    text <- data[i,'text']
    textall <- paste(data[i,'retweet_text'], data[i,'quoted_text'], data[i,'text'], sep = " ")
    hash <- str_extract_all(textall, "#\\S+")
    ment <- str_extract_all(textall, "@\\S+") 
    text = gsub("&amp", "", text)
    text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
    text = gsub("@\\w+", "", text)
    text = gsub("[[:punct:]]", "", text)
    text = gsub("[[:digit:]]", "", text)
    text = gsub("http\\w+", "", text)
    text = gsub("[ \t]{2,}", "", text)
    text = gsub("^\\s+|\\s+$", "", text) 
    word_count <- sapply(strsplit(text, " "), length)
    sent <- sentiment(text)
    meansent <- mean(sent$sentiment)
    sentiment <- round(meansent, digits = 4)
    hashtags <- list(hash[[1]])                                                             #Make list of all mentioned Hashtags
    mentions_screen_name <- ifelse(list(ment[[1]]) == "character(0)", " ", list(ment[[1]])) #Make ist of all mentioned Uers
    
    resi[i,'sentiment'] <- sentiment
    resi$hashtags[i] <- list(hashtags[[1]])                         #Format df$var[row_n] due to list input
    resi$mentions_screen_name[i] <- list(mentions_screen_name[[1]]) #Format df$var[row_n] due to list input
    resi[i, 'word_count'] <- word_count
    resi[i,]
    
  }
  
  on.exit(parallel::stopCluster(clu))
  
  data$sentiment <- results[,1]
  data$hashtags <- results[,2]                #Make list of all mentioned Hashtags                
  data$mentions_screen_name <- results[,3]    #Make ist of all mentioned Uers
  data$word_count <- results[,4]
  
  
  #Mentions Names character(0) to " "
  data$mentions_screen_name[data$mentions_screen_name == "character(0)"] <- " "
  
  #Age of Account NUMERIC (days) & Get number of Hashtags / Mentions (Parallel)
  clu <- makeCluster(nodes)
  registerDoParallel(clu)
  
  numoit <- nrow(data)
  res <- data.frame(matrix(NA, ncol = 3, nrow = numoit))
  colnames(res) <- c("account_age", "n_hashtags", "n_mentions")
  
  result <- foreach(i = 1:numoit, .combine = "rbind", .packages = c("tcltk")) %dopar% {
    if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=numoit)
    setTkProgressBar(pb, i)
    data_crea <- data[i, 'account_created_at']
    account_age <- as.numeric(as.Date("2018-11-13") - as.Date(data_crea[[1]]))
    n_hashtags <- sapply(strsplit(as.character(data[i,'hashtags']), " "), length)             #Number of Hashtags in Tweet --> NUMERIC
    n_hashtags[is.na(data[i,'hashtags'])] <- 0
    n_mentions<- sapply(strsplit(as.character(data[i,'mentions_screen_name']), " "), length)  #Number of Mentions in Tweet --> NUMERIC
    n_mentions[is.na(data[i,'mentions_screen_name'])] <- 0
    res[i,] <- cbind(account_age,n_hashtags,n_mentions)
  }
  
  on.exit(parallel::stopCluster(clu))
  
  data$account_age <- result[,1]
  data$n_hashtags <- result[,2]     #Number of Hashtags in Tweet --> NUMERIC                
  data$n_mentions <- result[,3]     #Number of Mentions in Tweet --> NUMERIC
  
  #Remove all Columns which have been used and are no longer important since they are made new as 
  #numerics and are no longer used for the later analysis:
  cnames2 <- c("ext_media_expanded_url", "description", "source",
               "reply_to_screen_name", "quoted_text",
               "retweet_text", "profile_expanded_url", "profile_image_url", "profile_background_url",
               "profile_banner_url", "text", "place_name", "is_quote", "country_code")
  data <- data %>% select(-one_of(cnames2)) 
  return(data)
  parallel::stopCluster(clu)
}

# Third processing steps including aggregated tweet informations add ratio variables ...
prepfunstreamdata3 <- function(data){
  data <- data %>% 
    group_by(user_id) %>% 
    mutate(mean_sentiment = mean(sentiment),
           median_sentiment = median(sentiment),
           sd_sentiment = sd(sentiment),
           mean_word_count = mean(word_count),
           median_word_count = median(word_count),
           sd_word_count = sd(word_count),
           mean_n_hashtags = mean(n_hashtags),
           median_n_hashtags = median(n_hashtags),
           sd_n_hashtags = sd(n_hashtags),
           mean_n_mentions = mean(n_mentions),
           median_n_mentions = median(n_mentions),
           sd_n_mentions = sd(n_mentions),
           retweet_ratio = mean(is_retweet),
           URL_ratio = mean(media_url),
           reply_ratio = mean(is_reply))
  
  # ADD Ratio Variables to streamed data frame:
  ## Friends Follower Ratio:
  data$friends_follower_ratio <- data$friends_count/data$followers_count 
  
  ### summary(data$friends_follower_ratio)
  ### Check what values to give NA's:
  ### table <- data %>% filter(friends_follower_ratio == 0 | is.na(friends_follower_ratio)) %>% group_by(Bot, friends_follower_ratio) %>% summarize(n = n())
  
  ### Both 0 and NA are all Bots! Therefore I change the NA's to Zero as well! 
  data$friends_follower_ratio[is.na(data$friends_follower_ratio)] <- 0
  
  ## Favourites Statuses Ratio:
  data$favourites_statuses_ratio <- data$favourites_count/data$statuses_count
  ###summary(data$favourites_statuses_ratio)
  ### No action needed!
  
  ## Listed Statuses Ratio:
  data$listed_statuses_ratio <- data$listed_count/data$statuses_count
  ###summary(data$listed_statuses_ratio)
  ### No action needed!
  
  ## Tweets per Day:
  data$tweets_per_day <- data$statuses_count/data$account_age
  ###summary(data$tweets_per_day)
  ### No action needed!
  
  # Verifed Variable to 0 and 1 all NA's are non verified accounts
  data$verified[is.na(data$verified)] <- 0
  
  # Save Data Frame with all variables including the ratio-variables 
  colnames(data) <- iconv(colnames(data), to='ASCII', sub='')
  
  return(data)
}

# 4) Process training data
###################################################################################################
## 4.1) Load & combine training data
###################################################################################################
# Set Dir to Genuine Data 
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data/cresci-2017.csv/datasets_full.csv/genuine_accounts.csv")

# Load Genuine User Data
genuine_df <- read_csv("users.csv")
genuine_df <- genuine_df[,1:40]
genuine_df$Bot <- 0 #No BOT
genuine_df$user_id <- genuine_df$id
genuine_df$id <- NULL

# Load Tweets from Genuine Users
genuine_tweets_df <- read_csv("tweets.csv")
genuine_tweets_df <-genuine_tweets_df[!duplicated(genuine_tweets_df$id),]

# Set Dir to Social Spambots 1
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data/cresci-2017.csv/datasets_full.csv/social_spambots_1.csv")

# Load Spambots 1 User Data
social_spam_1_df <- read_csv("users.csv")
social_spam_1_df <- social_spam_1_df[,1:40]
social_spam_1_df$Bot <- 1 # Bot
social_spam_1_df$user_id <- social_spam_1_df$id
social_spam_1_df$id <- NULL

# Load Tweets from Spambots 1 Users
social_spam_1_tweets_df <- read_csv("tweets.csv")
social_spam_1_tweets_df <-social_spam_1_tweets_df[!duplicated(social_spam_1_tweets_df$id),]

# Set Dir to Social Spambots 2
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data/cresci-2017.csv/datasets_full.csv/social_spambots_2.csv")

# Load Spambots 2 User Data
social_spam_2_df <- read_csv("users.csv")
social_spam_2_df <- social_spam_2_df[,1:40]
social_spam_2_df$Bot <- 1 # Bot
social_spam_2_df$user_id <- social_spam_2_df$id
social_spam_2_df$id <- NULL

# Load Tweets from Spambots 2 Users
social_spam_2_tweets_df <- read_csv("tweets.csv")
social_spam_2_tweets_df <-social_spam_2_tweets_df[!duplicated(social_spam_2_tweets_df$id),]

# Set Dir to Social Spambots 3
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data/cresci-2017.csv/datasets_full.csv/social_spambots_3.csv")

# Load Spambots 3 User Data
social_spam_3_df <- read_csv("users.csv")
social_spam_3_df <- social_spam_3_df[,1:40]
social_spam_3_df$Bot <- 1 # Bot
social_spam_3_df$user_id <- social_spam_3_df$id
social_spam_3_df$id <- NULL

# Load Tweets from Spambots 3 Users
social_spam_3_tweets_df <- read_csv("tweets.csv")
social_spam_3_tweets_df <-social_spam_3_tweets_df[!duplicated(social_spam_3_tweets_df$id),]

# Set Dir to Fake Followers
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data/cresci-2017.csv/datasets_full.csv/fake_followers.csv")

# Load Fake Followers User Data
fake_followers_df <- read_csv("users.csv")
fake_followers_df <- fake_followers_df[,1:39]
fake_followers_df$Bot <- 1 # Bot
fake_followers_df$user_id <- fake_followers_df$id
fake_followers_df$id <- NULL

# Load Tweets from Fake Follower Users
fake_followers_tweets_df <- read_csv("tweets.csv")
fake_followers_tweets_df <-fake_followers_tweets_df[!duplicated(fake_followers_tweets_df$id),]

# Set Dir to Social Traditional Spambots:
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data/cresci-2017.csv/datasets_full.csv/traditional_spambots_1.csv")

# Load Fake Traditional Spambots User Data
trad_spam_bot_df_1 <- read_csv("users.csv")
trad_spam_bot_df_1 <- trad_spam_bot_df_1[,1:39]
trad_spam_bot_df_1$Bot <- 1 # Bot
trad_spam_bot_df_1$user_id <- trad_spam_bot_df_1$id
trad_spam_bot_df_1$id <- NULL

# Load Tweets from Traditional Spambots Users
trad_spam_bot_tweets_df_1 <- read_csv("tweets.csv")
trad_spam_bot_tweets_df_1 <-trad_spam_bot_tweets_df_1[!duplicated(trad_spam_bot_tweets_df_1$id),]

# Combine all classifed Data Frames from Cresci et al. 2017 
# Combine User data with tweet data 
genuine_df_m <- merge(genuine_tweets_df, genuine_df, by = 'user_id')
social_spam_1_df_m <- merge(social_spam_1_tweets_df, social_spam_1_df, by = 'user_id')
social_spam_2_df_m <- merge(social_spam_2_tweets_df, social_spam_2_df, by = 'user_id')
social_spam_3_df_m <- merge(social_spam_3_tweets_df, social_spam_3_df, by = 'user_id')
fake_followers_df_m<- merge(fake_followers_tweets_df,fake_followers_df,by = 'user_id')
trad_spam_bot_df_1_m<- merge(trad_spam_bot_tweets_df_1,trad_spam_bot_df_1,by = 'user_id')

# Combine all social bot / fake_follower data frames and the genuine users data
trainingdnnDF_raw <- rbind(genuine_df_m, social_spam_1_df_m, social_spam_2_df_m, social_spam_3_df_m)
trainingdnnDF <-trainingdnnDF_raw[!duplicated(trainingdnnDF_raw$id),]

# Set Dir
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data")

#Save Training Data Frame 
saveRDS(trainingdnnDF, "TrainingDataRaw.rds")

# Clear Ram 
rm(social_spam_1_tweets_df, social_spam_1_df, social_spam_2_tweets_df, social_spam_2_df, 
   social_spam_3_tweets_df, social_spam_3_df, social_spam_1_df_m, social_spam_2_df_m, 
   social_spam_3_df_m, fake_followers_df_m, fake_followers_df, fake_followers_tweets_df,
   trad_spam_bot_df_1_m, trad_spam_bot_df_1, trad_spam_bot_tweets_df_1, genuine_df, genuine_df_m, genuine_tweets_df)
gc()
###################################################################################################
## 4.2) Process the training data
###################################################################################################
# Set Dir
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data")

# Load Training Data:
trainingdnnDF <- read_rds("TrainingDataRaw.rds")

# Prep Function Step 1:
data <- prepfuntraindata1(trainingdnnDF)

# Prep Function Step 2:
data <- prepfuntraindata2(data)

# Prep Function Step 3:
# Split data into parts of 100'000 rows for better handling (needs less ram when making clusters)
nr <- nrow(data)
n <- 100000
dflist <- split(data, rep(1:ceiling(nr/n), each=n, length.out=nr))
outtrain <- data.frame()

# Process Data (Text Analysis)
a <- Sys.time()
for(i in 1:length(dflist)){
  tmp <- prepfuntraindata3(dflist[[i]], nodes = 14)
  outtrain <- rbind(outtrain,tmp)
  rm(tmp)
  gc()
  cat(i, "th Element of List processed!\n")
  Sys.sleep(10)
}
saveRDS(outtrain, "TrainingDataPro_half.rds")
rm(dflist)
b <- Sys.time()
c3 <-b-a
cat("Time needed for ", nr, " rows: ",c3 )

#Prep Function Setp 4: 
outtrain <- readRDS("TrainingDataPro_half.rds")
outtrain <- prepfuntraindata4(outtrain)

## Get descriptives of all variables
stargazer(outtrain, type ="text", summary = TRUE)

#Transform Variables to factor type:
outtrain$Bot <- as.factor(outtrain$Bot)
outtrain$user_id <- as.numeric(outtrain$user_id)
outtrain$verified <- as.factor(outtrain$verified)
outtrain$is_reply <- as.factor(outtrain$is_reply)
outtrain$profile_banner_default <- as.factor(outtrain$profile_banner_default)
outtrain$profile_image_default <- as.factor(outtrain$profile_image_default)
outtrain$is_retweet <- as.factor(outtrain$is_retweet)
outtrain$profile_background_default <- as.factor(outtrain$profile_background_default)
outtrain$geo_enabled <- as.factor(outtrain$geo_enabled)
outtrain$media_url <- as.factor(outtrain$media_url)

# Remove the other NA's for the factor variable of is_retweet!
outtrain <- filter(outtrain, !is.na(is_retweet))

sapply(outtrain, class)

# Order by User_id 
outtrain <- outtrain[order(outtrain$user_id),]
###################################################################################################
## 4.3) Save Data
###################################################################################################
# Save Training Data on the tweet level:
outtrain <- as.data.frame(outtrain)
saveRDS(outtrain, "TrainingDataPro_Tweet.rds")
write_csv(outtrain, "TrainingDataPro_Tweet.csv")

# Save Training Data on the User Level:
# Aggregate by User_id:
# Since I have already all the aggregated data in the data set I remove all features which are 
# related to the status_id and then remove the duplicates!
cols_status <- c("status_id", "is_retweet", "is_reply", "media_url", "sentiment", "word_count", 
                 "n_hashtags", "n_mentions" )

outtrain_user <- outtrain %>% select(-one_of(cols_status)) 
#Get number of unique users 
sapply(outtrain_user, function(x) length(unique(x)))
#Remove Dublicates
outtrain_user <-outtrain_user[!duplicated(outtrain_user$user_id),]
#Check if number of unique users is still the same:
sapply(outtrain_user, function(x) length(unique(x)))

outtrain_user <- as.data.frame(outtrain_user)
saveRDS(outtrain_user, "TrainingDataPro_User.rds")
write_csv(outtrain_user, "TrainingDataPro_User.csv")
###################################################################################################
# 5) Process collected data
###################################################################################################
## 5.1) Get path of all JSON 
###################################################################################################
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Raw")

# Get list of all files with the tweets collected from the midterm election
path <- paste(getwd())
filenames <- list.files(path, pattern = "*.rds", full.names = T)
filenames
###################################################################################################
## 5.2) Process Collected data
###################################################################################################
# Run Preprocessing in bulk over all files while saving each file again as a prepocessed file:
month <- rep(c(10,11), each = 26)
day <- c(seq(06,31,by=1), seq(01,27, by=1))
k <- 1

# Process data for each single day in the folder 
# This loop is ver slow since it has to do lots of processing steps like text analysis and others for 
# all 28.6 Million tweets collected over 39 days
for(i in 1:39){
  setwd(path)
  tmp <- read_rds(filenames[i])
  tmp <- prepfunstreamdata1(tmp)
  nr <- nrow(tmp)
  n <- 200000
  dflist <- split(tmp, rep(1:ceiling(nr/n), each=n, length.out=nr))
  tmpout <- data.frame()
  for(j in 1:length(dflist)){
    tmpin <- prepfunstreamdata2(dflist[[j]], nodes = 8)
    tmpout <- rbind(tmpout,tmpin)
    rm(tmpin)
    gc()
    cat(j, "th Element of List processed!\n")
    Sys.sleep(30)
  }
  setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Processed")
  saveRDS(tmpout, paste0("Tweets_Prep_",month[k],"_",day[k],".rds"))
  rm(tmp, tmpout)
  cat("Stream from ", month[k], " ", day[k], " has been processed\n")
  k <- k + 1
  gc()
}
###################################################################################################
## 5.3) Combine all Days into one single Data Frame
###################################################################################################
# Set Dir
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Processed")

# Get list of File Paths:
path <- paste(getwd())
processedfiles <- list.files(path, pattern = "*.rds", full.names = T)
processedfiles

# Make an empty Data Frame:
tweetsdf <- data.frame()

# Load and attacht each Day to the empty data frame
for(i in 1:length(processedfiles)){
  tmp <- readRDS(processedfiles[i])
  tweetsdf <- rbind(tweetsdf, tmp)
  cat("Importet file number ", i, " !\n")
  
}

# Get tweets of User_Id's in Data:
n_users <- tweetsdf %>% group_by(user_id) %>% summarize(count=n())
# summary
summary(n_users$count)

# Correct mistake with n_mentions
summary(tweetsdf$n_mentions)
tweetsdf$n_mentions <- ifelse(tweetsdf$mentions_screen_name == " ", 0, tweetsdf$n_mentions)

# Save all 
saveRDS(tweetsdf, "All_Tweets_Processed_Combined.rds")
###################################################################################################
## 5.4) Make data frame for predictions by user and data frame by tweets with descriptive satistics
###################################################################################################
# Set Dir
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Processed")
# Load combined data frame
tweetsdf <- readRDS("All_Tweets_Processed_Combined.rds")
###################################################################################################
### 5.4.1) Calculate Descriptives
###################################################################################################
tweetsdf <- prepfunstreamdata3(tweetsdf)

# As a consequence of the ratio calculations in the function before some features contain infinite values which have to be imputed. 
# With the R package  Hmisc, infinite (not available = NAs) values can be substituted by the mean. It is not 
# possible to use regression like imoutation due to the size of the data (takes far to much time (several days))
tweetsdf$reply_ratio <- ifelse(is.na(tweetsdf$sd_word_count), NA, tweetsdf$reply_ratio)
tweetsdf$retweet_ratio <- ifelse(is.na(tweetsdf$sd_word_count), NA, tweetsdf$retweet_ratio)
tweetsdf$URL_ratio <- ifelse(is.na(tweetsdf$sd_word_count), NA, tweetsdf$URL_ratio)

tweetsdf[is.na(tweetsdf)] <- NA

tweetsdf <- data.table(tweetsdf)
for(j in 22:ncol(tweetsdf)) set(tweetsdf, which(is.infinite(tweetsdf[[j]])), j, NA)

# imputation of NAs
# imputed with Hmisc package
# Mean imoutation is not as fany as with regression but much faster and still working good enough for Classification via a DNN
varswithNA <- c("reply_ratio", "retweet_ratio", "URL_ratio", "mean_sentiment", "median_sentiment",      
                "sd_sentiment", "mean_word_count", "median_word_count", "sd_word_count",             
                "mean_n_hashtags", "median_n_hashtags", "sd_n_hashtags", "mean_n_mentions",          
                "median_n_mentions","sd_n_mentions", "retweet_ratio", "URL_ratio",                 
                "reply_ratio", "friends_follower_ratio", "favourites_statuses_ratio","listed_statuses_ratio",     
                "tweets_per_day")

for(v in varswithNA){
  exe <- paste0("tweetsdf$",v, "<- impute(tweetsdf$",v, ", mean)")   # replace with mean
  eval(parse(text=exe))
}

stargazer(tweetsdf, type ="text", summary = TRUE)

# Transform Variables to factor type:
tweetsdf$user_id <- as.numeric(tweetsdf$user_id)
tweetsdf$verified <- as.factor(tweetsdf$verified)
tweetsdf$is_reply <- as.factor(tweetsdf$is_reply)
tweetsdf$profile_banner_default <- as.factor(tweetsdf$profile_banner_default)
tweetsdf$profile_image_default <- as.factor(tweetsdf$profile_image_default)
tweetsdf$is_retweet <- as.factor(tweetsdf$is_retweet)
tweetsdf$profile_background_default <- as.factor(tweetsdf$profile_background_default)
tweetsdf$geo_enabled <- as.factor(tweetsdf$geo_enabled)
tweetsdf$media_url <- as.factor(tweetsdf$media_url)

sapply(tweetsdf, class)

# Order by User_id 
tweetsdf <- tweetsdf[order(tweetsdf$user_id),]
###################################################################################################
### 5.4.2) Save data on the tweet level
###################################################################################################
tweetsdf <- as.data.frame(tweetsdf)
saveRDS(tweetsdf, "Election_Stream_by_tweets_for_class.rds")
###################################################################################################
### 5.4.3) Save data on the user level
###################################################################################################
# Aggregate by User_id:
# Since I have already all the aggregated data in the data set I remove all features which are 
# related to the status_id and then remove the duplicates!
cols_status <- c("is_retweet", "is_reply", "media_url", "sentiment", "word_count", 
                 "n_hashtags", "n_mentions" )

tweetsdf_user <- tweetsdf %>% select(-one_of(cols_status)) 

# Get number of unique users 
sapply(tweetsdf_user, function(x) length(unique(x)))

#Remove Dublicates while keeping themost recent  meta data 
tweetsdf_user <- tweetsdf_user %>% group_by(user_id) %>% slice(which.max(status_id))

tweetsdf_user$status_id <- NULL
tweetsdf_user <-tweetsdf_user[!duplicated(tweetsdf_user$user_id),]

# Check if number of unique users is still the same:
sapply(tweetsdf_user, function(x) length(unique(x)))

# Save User Data for Prediction 
tweetsdf_user <- as.data.frame(tweetsdf_user)
saveRDS(tweetsdf_user, "Election_Stream_by_user_for_class.rds")
###################################################################################################