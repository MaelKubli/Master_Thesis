###################################################################################################
# Classify Tweets with DNN Model:
###################################################################################################
# Description
###################################################################################################
# This script includes the code needed to first add classification variables made with the hashtags 
# used to collect the data. This includes if a tweet is considered to be in favour of Republicans 
# or Democrats or UndecidableFurthermore it also includes if a Tweet was published for one of the 
# 19 specific districts I looked at with some extra hashtags defined for them or if it is a tweet 
# for either the whole state or the whole country. Secondly this script includes the classification 
# of the users type (Bot or Not) with the best model found durning training with H2O.

# Caution to work with the Data of tweets you need at least 64 GB RAM otherwise you will encounter 
# errors due to insufficient memory size!
###################################################################################################
# Content
###################################################################################################
# 1) Dependencies
# 2) Load Data of Tweets
# 3) Classification with Hashtags
## 3.1) Classify if tweet is from one of the districts I am interested in or not
## 3.2) Classify if Tweet is referencing a District or not
## 3.3) Classify if a Hashtag is either Democratic or Republican or Undecidable
# 4) Save Data of Tweets 
# 5) Load Data of Users
# 6) Classify User data with H2O
## 6.1) Prepare Data for Prediction
## 6.2) Start H2O
## 6.3) Load Model
## 6.4) Predict with Model
## 6.5) Save User Data with Prediction
## 6.6) Add Prediction of User to Tweets Data
## 6.7) Terminate H2O
###################################################################################################
# 1) Dependencies
###################################################################################################
library(h2o)
library(readr)
library(data.table)
library(bit64)
library(dplyr)
library(car)
library(stringi)
library(stringr)
library(parallel)
library(doParallel)
library(foreach)
library(tcltk)
library(iterators)
library(textclean)
library(stargazer)
library(ggplot2)
library(scales)
library(magrittr)
library(reshape2)
###################################################################################################
# 2) Load Data of Tweets
###################################################################################################
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Processed")
tweetsdf <- readRDS("Election_Stream_by_tweets_for_class.rds")
###################################################################################################
# 3) Classification with Hashtags
###################################################################################################
## 3.1) Classify if tweet is from one of the districts I am interested in or not
###################################################################################################
# All hashtags used for the different districts in California
districtCA10 <- c("#CA10","#JeffDenham","#JoshHarder","#DoNothingAngryDenham","#DefeatDenham","#DumpDenham","#BayAreaHarder")
districtCA25 <- c("#CA25","#SteveKnight","#KatieHill","#gethilltothehill","#knightout","#VoteKnightOut","#VoteForKnight")
districtCA39 <- c("#CA39","#YoungKimCD39","#TeamYoung","#GillCisnerosCA","#FlipOCBlue","#GilontheHill","#cisneros4congress")
districtCA48 <- c("#CA48","#DanaRohrabacher","#HarleyRouda","#TeamHarley","#Rohrabacher","#VoteHarley","#HireHarley","#VoteForDana")

# All hashtags used for the different districts in Colorado
districtCO06 <- c("#CO06","#MikeCoffman","#JasonCrow","#TeamCrow","#VoteCrow","#TeamCoffman","#VoteCoffman")

# All hashtags used for the different districts in Florida
districtFL16 <- c("#FL16","#VernBuchanan","#DavidShapiro","#redtidevern","#VernsYacht")
districtFL18 <- c("#FL18","#TeamBaer","#BrianMast","#laurenbaer","#BrianMastIsToxic","#TeamMast","#MastforCongress")

# All hashtags used for the different districts in Texas
districtTX07 <- c("#TX07","#CongCulberson","#JohnCulberson","#TeamLizzie","#Lizzie4Congress","#VoteOutCulberson","#VoteCulberson")
districtTX32 <- c("#TX32","#TeamCollin","#collinallred","#collinallredTX","#PeteSessions","#VotePeteSessions")
districtTX23 <- c("#TX23","#WillHurd","#GinaJones","#TX23forGina","#Gina2018","#HurdforCongress","#hurdonthehill")

# All hashtags used for the different districts in New York
districtNY19 <- c("#NY19","#DelgadoforNY19","#AntonioDelgado","#JohnFasoNY","#TeamFaso","#FireFaso")
districtNY22 <- c("#NY22","#ABrindisiNY","#ClaudiaTenney","#BrindisiforCongress","#onetermtenney","#TenneyforCongress")

# All hashtags used for the different districts in Ilinois
districtIL12 <- c("#IL12","#KellyCoalition","#RepBost","#FlipIL12","#VoteKelly","#MikeBost")

# All hashtags used for the different districts in Kansas
districtKS03 <- c("#KS03","#KevinYoder","#ShariceDavids","#YoderVoter","#VoteDavids")
districtKS02 <- c("#KS02","#SteveWatkins","#Steve4Kansas","#PaulDavis","#PaulDavisKS")

# All hashtags used for the different districts in Main
districtMN01 <- c("#MN01","#TeamFeehan","#DanFeehan","#DanielFeehan","#JimHagedornMN","#Jimhegedorn")
districtMN08 <-c ("#MN08","#PeteStauber","#JoeRadinovich","#VoteStauber","#JoeRadical")

# All hashtags used for the different districts in Ohio
districtOH12 <- c("#OH12","#TroyBalderson","#DannyOConnor","#VoteOConnor","#VoteBalderson")
districtOH01 <- c("#OH01","#SteveChabot","#AftabPureval","#Chabotage","#TeamAftab","#TeamChabot","#VoteChabot","#VoteAftab")

#Classify Tweets:
tweetsdf_sub$District <- "none"
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtCA10, collapse="|"), x, ignore.case = T))}), "CA10", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtCA25, collapse="|"), x, ignore.case = T))}), "CA25", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtCA39, collapse="|"), x, ignore.case = T))}), "CA39", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtCA48, collapse="|"), x, ignore.case = T))}), "CA48", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtCO06, collapse="|"), x, ignore.case = T))}), "CO06", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtFL16, collapse="|"), x, ignore.case = T))}), "FL16", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtFL18, collapse="|"), x, ignore.case = T))}), "FL18", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtTX07, collapse="|"), x, ignore.case = T))}), "TX07", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtTX32, collapse="|"), x, ignore.case = T))}), "TX32", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtTX23, collapse="|"), x, ignore.case = T))}), "TX23", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtNY19, collapse="|"), x, ignore.case = T))}), "NY19", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtNY22, collapse="|"), x, ignore.case = T))}), "NY22", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtIL12, collapse="|"), x, ignore.case = T))}), "IL12", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtKS03, collapse="|"), x, ignore.case = T))}), "KS03", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtKS02, collapse="|"), x, ignore.case = T))}), "KS02", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtMN01, collapse="|"), x, ignore.case = T))}), "MN01", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtMN08, collapse="|"), x, ignore.case = T))}), "MN08", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtOH12, collapse="|"), x, ignore.case = T))}), "OH12", tweetsdf$District)
tweetsdf$District <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(districtOH01, collapse="|"), x, ignore.case = T))}), "OH01", tweetsdf$District)
###################################################################################################
## 3.2) Classify if Tweet is referencing a District or not
###################################################################################################
tweetsdf$National <- ifelse(tweetsdf$District ==  "none", 1, 0)
###################################################################################################
## 3.3) Classify if a Hashtag is either Democratic or Republican or Undecidable
###################################################################################################

Republican <- c("#Republicans","#VoteRed","#GOP","#MAGA","#MakeAmericaGreatAgain","#RedTsunami",
                "#GreatAwakening","#unhinged","#WalkawayCampaign","#WalkAway","#Republicans2018",
                "#democratspartyofhate","#LiberalMob","#KeepItRed","#RemainRed")

Democrats <- c("#Democrats","#VoteBlue","#Bluewave","#MASA","#MakeAmericsSmartAgain","#DumpTrump",
               "#DemsWork4USA","#YesWeCan","#FlipItBlue","#Democrats2018","#WinBlue","#FlipTheHouse",
               "#RedToBlue", "#takeitback")

tweetsdf$Republican <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(Republican, collapse="|"), x, ignore.case = T))}), 1, 0)
tweetsdf$Democrats <- ifelse(lapply(tweetsdf$hashtag, function(x){any(grepl(paste(Democrats, collapse="|"), x, ignore.case = T))}), 1, 0)

# Tweets which contain both hashtags from the republicans and democrats are overwritten to no specific group:
tweetsdf$Democrats <- ifelse(tweetsdf$Republican == 1, 0, tweetsdf$Democrats)
tweetsdf$Republican <- ifelse(tweetsdf$Democrats == 1, 0, tweetsdf$Republican)

View(head(tweetsdf,10000))
###################################################################################################
# 4) Save Data of Tweets in the original File (there is no need to make a new data set of several GB)
###################################################################################################
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Processed")
saveRDS(tweetsdf, "Election_Stream_by_tweets_for_class.rds")
###################################################################################################
# 5) Load Data of Users
###################################################################################################
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Processed")
usersdf <- readRDS("Election_Stream_by_user_for_class.rds")
###################################################################################################
# 6) Classify User data with H2O
###################################################################################################
###################################################################################################
## 6.1) Prepare Data for Prediction
###################################################################################################
usersdf$created_at <- NULL
usersdf$hashtags <- NULL
usersdf$mentions_screen_name <- NULL
usersdf$quoted_user_id <- NULL
usersdf$retweet_user_id <- NULL
usersdf$status_url <- NULL
usersdf$account_created_at <- NULL

usersdf$user_id <- as.character(usersdf$user_id)

write_csv(usersdf, "Election_Stream_by_user_for_class.csv")
###################################################################################################
## 6.2) Start H2O
###################################################################################################
# Start an H2O using one node with all cores minus one core and as much GB of RAM as you have. 
#I use 16 cores with 32 GB but it will also work with less cores and only 4 GB of RAM. 
# The downside then is that the training takes much longer for the same amount of models 

# Leave at least one core for your system mabe even two if you want to do other stuff on it. 
h2oServer <- h2o.init(ip="localhost", port=54321, max_mem_size="4g", nthreads=4)
###################################################################################################
## 6.3) Load Model and Data
###################################################################################################
# Path to the best model
model_path <- "D:\\CloudStation\\Universitaet Zuerich\\Master Thesis\\Output\\GridSearch_Step_2_v2\\dl_grid_model_77"
model_path <- "/Users/Mael/CloudStation/Universitaet Zuerich/Master Thesis/Output/GridSearch_Step_2_v2/dl_grid_model_33"

# Load best Model int H2O
best_model <- h2o.loadModel(model_path)

# Change dir to path where the data set to predict is: /Users/Mael/
workdir="D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Processed"
setwd(workdir)

# Load Data to predict on the User Level, since it would not make much sense to predict each tweet by it self, 
# because a user is either a bot or he is not. This can't change between tweets or at least it shouldn't.  /Users/Mael/
data <- h2o.importFile(path = "D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Processed/Election_Stream_by_user_for_class.csv", destination_frame = "data")

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
## 6.4) Predict with Model
###################################################################################################
midtermusers.fit = h2o.predict(object = best_model, newdata = data)
summary(midtermusers.fit)

predictions <- as.data.frame(midtermusers.fit)
###################################################################################################
## 6.5) Save User Data with Prediction
###################################################################################################
# Bind the two data sets in h2o
data_pred_data <- h2o.cbind(data, midtermusers.fit)

# Extract the user_id and the prediction variables
data_pred_data_df <- data_pred_data[, c("user_id", "predict", "p0", "p1")]

# Merge it with the original classification data
data_backup <- read.csv("Election_Stream_by_user_for_class.csv")
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

# Save the Data:
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Predicted_Data")
saveRDS(data_pred_data_df, "Users_Predicted_w-9.rds")

# The following histogramm shows the distribution of attributed scores per classified user.
plot <- ggplot(data_pred_data_df) +
  geom_histogram(aes(y=stat(width*density), x=p1), color = "black", fill = "grey", bins = 100) +
  labs(title = "Distribution of attributed scores per classified user account", y = "Share of accounts [%]", x = "Predicted probability of account being a social bot [%]") +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0),
                     limits = c(0,0.10)) +
  scale_x_continuous(labels = percent_format()) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("dist_predict_bot.png", plot = plot, device = "png",
       width = 8, height = 5, dpi = 300)

###################################################################################################
## 6.6) Add Prediction of User to Tweets Data
###################################################################################################
# Load Processed Collected Tweets Data 
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Processed")
tweetsdf <- readRDS("Election_Stream_by_tweets_for_class.rds")

# Prepare predicted user data for mergeing with user data
data_pred_data_df <- data_pred_data_df[, c("user_id", "predict", "p0", "p1")]

# Remove columns which I no longer need: 
tweetsdf$name_digits <- NULL
tweetsdf$name_length <- NULL
tweetsdf$screen_name_digits <- NULL
tweetsdf$screen_name_length <- NULL
tweetsdf$description_digits <- NULL
tweetsdf$description_length <- NULL
tweetsdf$media_url <- NULL

# Merge
tweetsdf <- merge(tweetsdf, data_pred_data_df, by = c("user_id"), all = T)

# Save Tweets Predicted
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Predicted_Data")
saveRDS(tweetsdf, "Tweets_Predicted_w-9.rds")
###################################################################################################
## 6.7) Terminate H2O
###################################################################################################
h2o.shutdown(prompt = FALSE)
###################################################################################################
