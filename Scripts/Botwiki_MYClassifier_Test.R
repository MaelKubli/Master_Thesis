###################################################################################################
# Get Data from Botwiki and then get Data from Bots from Twitter if possible:
###################################################################################################
# Description:
# Get Example Bots from Botwiki to test effectivenes of Classifier:
###################################################################################################
# Content:
###################################################################################################
# 1) Dependencies
# 2) Classic html like Web-Scraper
## 2.1) Setup 
## 2.2) Obtain Twitter Names from all Bots in the Data Base
# 3) Get Tweets from the Twitter Accounts obtiained before
# 4) Processing functions for collected data
# 5) Processing Data
# 6) Save data on the user level
# 7) Classify Accounts
###################################################################################################
# 1) Dependencies
###################################################################################################
suppressMessages(library(bitops))
suppressMessages(library(xml2))
suppressMessages(library(httr))
suppressMessages(library(RCurl))
suppressMessages(library(rvest))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(readr)) #Need this to read csv files as data table has problems with titels with quotationmarks
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(jsonlite))
suppressMessages(library(rjson))
suppressMessages(library(purrr))
suppressMessages(library(rtweet))
suppressMessages(library(parallel))
suppressMessages(library(doParallel))
suppressMessages(library(itertools))
suppressMessages(library(iterators))
suppressMessages(library(foreach))
suppressMessages(library(stringi))
suppressMessages(library(stringr))
suppressMessages(library(bit64))
suppressMessages(library(dplyr))
suppressMessages(library(car))
suppressMessages(library(sentimentr))
suppressMessages(library(microbenchmark))
suppressMessages(library(tcltk))
suppressMessages(library(textclean))
suppressMessages(library(h2o))
suppressMessages(library(ggplot2))
suppressMessages(library(scales))
###################################################################################################
# 2) Classic html like Web-Scraper
###################################################################################################
###################################################################################################
## 2.1) Setup 
###################################################################################################
setwd("~/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data")


# Save URL from the webpage containing the links to the pressreleases:
url <- "https://botwiki.org/wp-json/wp/v2/bot?per_page=50&page="

# Sets timeout option for RCurl to 200 sec:
myOpts <- curlOptions(connecttimeout = 200)

# Builds an empty data frame for all links to the pressreleases:
df_tw_names <- data.frame()
df_tw_names_new <- data.frame()
###################################################################################################
## 2.2) Obtain Twitter Names from all Bots in the Data Base
###################################################################################################
for(i in 1:18){
  a <- GET(paste0(url,i))
  a <- content(a, as="text")
  # get rid of the non-JSON stuff...
  a <- gsub("^angular.callbacks._2\\(", "", a)
  a <- gsub("\\);$", "", a)
  tmp <- fromJSON(a)
  tmp <- as.data.frame(matrix(unlist(map(tmp[1:length(tmp)], "bot_urls")), nrow = length(tmp), byrow = T), stringAsFactor = FALSE)
  colnames(tmp) <- c("Twitter_Name_1", "Twitter_Name_2")
  df_tw_names <- rbind(df_tw_names, tmp)
  rm(tmp)
}
df_tw_names$Twitter_Name_1 <- as.character(df_tw_names$Twitter_Name_1)
df_tw_names$Twitter_Name_2 <- as.character(df_tw_names$Twitter_Name_2)

hel2 <- df_tw_names$Twitter_Name_2
hel1 <- df_tw_names$Twitter_Name_1 

hel <- c(hel1, hel2)

df_tw_names <- as.data.frame(hel)
colnames(df_tw_names) <- "Twitter_Name" 
df_tw_names$Twitter_Name <- as.character(df_tw_names$Twitter_Name)
df_tw_names$Twitter_Name <- gsub("https://twitter.com/", "", df_tw_names$Twitter_Name)
df_tw_names$Twitter_Name <- gsub("http://twitter.com/", "", df_tw_names$Twitter_Name)
df_tw_names$Twitter_Name <- gsub(".*@", "", df_tw_names$Twitter_Name)

df_tw_names <- df_tw_names[!grepl("http", df_tw_names$Twitter_Name),]
df_tw_names <- as.data.frame(df_tw_names)
colnames(df_tw_names) <- "Twitter_Name" 
df_tw_names$Twitter_Name <- as.character(df_tw_names$Twitter_Name)
###################################################################################################
# 3) Get Tweets from the Twitter Accounts obtiained before:
###################################################################################################
# Load API Key:
token <- readRDS("~/DigDemLab/Tokens/twitter_token_mael.rds")
# Get Timelines:
tweetsdf <- data.frame()
for(i in 1:nrow(df_tw_names)){
  users <- df_tw_names[i, 1]
  tmp <- get_timelines(user = users, n = 1000, token = token, check = TRUE, parse = T)
  tweetsdf <- rbind(tweetsdf, tmp)
  cat("Account number: ", i, " with user name ", users,"\n")
  if(i %in% c(150,300,450,600,750,900)){ Sys.sleep(901)}
}

tweetsdf$user_id <- as.character(tweetsdf$user_id)
tweetsdf$status_id <- as.character(tweetsdf$status_id)
tweetsdf$retweet_user_id <- as.character(tweetsdf$retweet_user_id)
tweetsdf$retweet_status_id <- as.character(tweetsdf$retweet_status_id)

setwd("~/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data")

write_rds(tweetsdf, "Botwiki_Data_Test_MYClassifier.rds")
rm(tweetsdf, tmp, users, url, i)
###################################################################################################
# 4) Processing functions for collected data
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
###################################################################################################
# 5) Processing Data
###################################################################################################
# Run Preprocessing in bulk over all files while saving each file again as a prepocessed file:
setwd("~/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data")
tmp <- read_rds("Botwiki_Data_Test_MYClassifier.rds")
sapply(tmp, class)
tmp <- prepfunstreamdata1(tmp)
nr <- nrow(tmp)
n <- 200000
dflist <- split(tmp, rep(1:ceiling(nr/n), each=n, length.out=nr))
tmpout <- data.frame()
  for(j in 1:length(dflist)){
    tmpin <- prepfunstreamdata2(dflist[[j]], nodes = 3)
    tmpout <- rbind(tmpout,tmpin)
    rm(tmpin)
    gc()
    cat(j, "th Element of List processed!\n")
    Sys.sleep(30)
  }

tweetsdf <- tmpout
rm(tmpout, nr, n, tmp)
gc()

n_users <- tweetsdf %>% group_by(user_id) %>% summarize(count=n())

# summary
summary(n_users$count)
# Correct mistake with n_mentions
summary(tweetsdf$n_mentions)
tweetsdf$n_mentions <- ifelse(tweetsdf$mentions_screen_name == " ", 0, tweetsdf$n_mentions)
#Apply third Processing Function
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
tweetsdf$user_id <- as.character(tweetsdf$user_id)
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
tweetsdf <- as.data.frame(tweetsdf)
#Save Processed Data:
setwd("~/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data")
saveRDS(tweetsdf, "Botwiki_Data_Test_MYClassifier_processed_tweet.rds")
###################################################################################################
# 6) Save data on the user level
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
saveRDS(tweetsdf_user, "Botwiki_Data_Test_MYClassifier_processed_user.rds")
###################################################################################################
# 7) Classify Accounts
###################################################################################################
## 7.1) Prepare Data for Prediction
###################################################################################################
usersdf <- tweetsdf_user
rm(tweetsdf_user)

usersdf$created_at <- NULL
usersdf$hashtags <- NULL
usersdf$mentions_screen_name <- NULL
usersdf$quoted_user_id <- NULL
usersdf$retweet_user_id <- NULL
usersdf$status_url <- NULL
usersdf$account_created_at <- NULL

usersdf$user_id <- as.character(usersdf$user_id)

write_csv(usersdf, "Botwiki_Data_Test_MYClassifier_processed_user.csv")
###################################################################################################
## 7.2) Start H2O
###################################################################################################
# Leave at least one core for your system mabe even two if you want to do other stuff on it. 
h2oServer <- h2o.init(ip="localhost", port=54321, max_mem_size="4g", nthreads=4)
###################################################################################################
## 7.3) Load Model and Data
###################################################################################################
# Path to the best model
model_path <- "D:\\CloudStation\\Universitaet Zuerich\\Master Thesis\\Output\\GridSearch_Step_2_v2\\dl_grid_model_77"
model_path <- "/Users/Mael/CloudStation/Universitaet Zuerich/Master Thesis/Output/GridSearch_Step_2_v2/dl_grid_model_77"

# Load best Model int H2O
best_model <- h2o.loadModel(model_path)

# Change dir to path where the data set to predict is: /Users/Mael/
workdir="D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data"
setwd(workdir)

# Load Data to predict on the User Level, since it would not make much sense to predict each tweet by it self, 
# because a user is either a bot or he is not. This can't change between tweets or at least it shouldn't.  /Users/Mael/
data <- h2o.importFile(path = "/Users/Mael/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data/Botwiki_Data_Test_MYClassifier_processed_user.csv", destination_frame = "data")

# Get dimension of data set:
dim(data)

# Change all other binary Vairables to factors as well to ensure that they are not seen as 
# intervals between 0 and 1
data$verified <- as.factor(data$verified)
data$is_reply <- as.factor(data$is_reply)
data$profile_banner_default <- as.factor(data$profile_banner_default)
data$profile_image_default <- as.factor(data$profile_image_default)
data$is_retweet <- as.factor(data$is_retweet)
data$profile_background_default <- as.factor(data$profile_background_default)
data$geo_enabled <- as.factor(data$geo_enabled)
data$media_url <- as.factor(data$media_url)
###################################################################################################
## 7.4) Predict with Model
###################################################################################################
midtermusers.fit = h2o.predict(object = best_model, newdata = data)
summary(midtermusers.fit)

# 65 Genuine Accounts (all false negatives)
# 328 Bot Accounts (all true positive)
# 0 Genuine Accounts (all true negative)
# 0 Bot Accounts (all false positive)

predictions <- as.data.frame(midtermusers.fit)
###################################################################################################
## 7.5) Save User Data with Prediction
###################################################################################################
data_pred_data <- h2o.cbind(data, midtermusers.fit)

# Extract the user_id and the prediction variables
data_pred_data_df <- data_pred_data[, c("user_id", "predict", "p0", "p1")]

# Merge it with the original classification data
data_backup <- read.csv("Botwiki_Data_Test_MYClassifier_processed_user.csv")
data_backup$user_id <- as.character(data_backup$user_id)

data_pred_data_df <- as.data.frame(data_pred_data_df)
colnames(data_pred_data_df) <- c("user_id_2", "predict", "p0", "p1")

data_backup$user_id <- as.character(data_backup$user_id)
data_pred_data_df$user_id_2 <- as.character(data_pred_data_df$user_id_2)

data_pred_data_df <- cbind(data_backup, data_pred_data_df)

# Check if all rows are rightfully matched
data_pred_data_df$Check <- NA
data_pred_data_df$Check <- ifelse(data_pred_data_df$user_id == data_pred_data_df$user_id_2, TRUE, FALSE)

before <- nrow(data_pred_data_df)
data_pred_data_df <- data_pred_data_df %>% filter(Check == TRUE) 
after <- nrow(data_pred_data_df)

before == after

# Remove check and user_id_2
data_pred_data_df$Check <- NULL
data_pred_data_df$user_id_2 <- NULL

write_rds(data_pred_data_df, "Botwiki_Data_Test_MYClassifier_classified.csv")
data_pred_data_df<- read_rds("Botwiki_Data_Test_MYClassifier_classified.csv")

# The following histogramm shows the distribution of attributed scores per classified user.
plot <- ggplot(data_pred_data_df) +
  geom_histogram(aes(y=stat(width*density), x=p1), color = "black", fill = "grey", bins = 100) +
  labs(title = "Distribution of attributed scores per classified user account", y = "Share of accounts [%]", x = "Predicted probability of account being a social bot [%]") +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0),
                     limits = c(0,0.7)) +
  scale_x_continuous(labels = percent_format()) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot


###################################################################################################
## 7.7) Terminate H2O
###################################################################################################
h2o.shutdown(prompt = FALSE)
###################################################################################################