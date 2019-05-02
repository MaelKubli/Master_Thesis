###################################################################################################
# Analysis of Data Part I 
###################################################################################################
# Description
###################################################################################################
# This Script contains the Analsyis for all descriptive statistics and the temporal analysis of the 
# data. 
# 
###################################################################################################
# Content
###################################################################################################
# 1) Dependencies
# 2) Load Data
# 3) Appendix Table of Hashtags 
# 4) Descriptive Plots
## 4.1) Descriptives Dataset before n < 10 removed
## 4.2) Bot Distribution and optimal Thershold na minmal n for classification
### 4.2.1) Prediction Distribution of Tweets from Bots
### 4.2.2) Prediction Distribution of Users from Bots
### 4.2.3.) Analysis of Bot Peak at 0.72 
#### 4.2.3.0) Variance Table 
#### 4.2.3.1) Principal comonent Analysis (Multiple Factor Analysis)
### 4.2.4) Table 1 MA Thesis
### 4.2.5) Remove all users and their tweets with n less than 10 
## 4.3) Tweets per Hour
### 4.3.1) Tweets per hour plot by Party
### 4.3.2) Avergae Tweets by Pary Afiliation...
### 4.3.3) Tweets per hour plot by Tweet Type (Bot or Not)
### 4.3.4) Average Tweets by Classification type 
### 4.3.5) Tweets per hour plot by Tweet Type and Party (Bot or Not) Number of Tweets per hour posted around the US Mideterms 2018 by Bots and Genuine Accounts
### 4.3.6) Average Tweets by National/District and type 
### 4.3.7) Tweets per hour plot by Tweet Type and Party (Bot or Not) Small Multiple Nice
# 5) Model Features and Metrics
# 6) Descriptive Tables
## 6.1) Table 10 MA Thesis
## 6.2) Table  MA Thesis
###################################################################################################
# 1) Dependencies
###################################################################################################
library(readr)
library(rtweet)
library(data.table)
library(bit64)
library(dplyr)
library(car)
library(stringi)
library(stringr)
library(stargazer)
library(ggplot2)
library(ggsci)
library(ggExtra)
library(ggridges)
library(ggrepel)
library(ggalt)
library(scales)
library(reshape2)
library(h2o)
library(lubridate)
library(stats)
library(factoextra)
library(FactoMineR)
###################################################################################################
# 2) Load Data
###################################################################################################
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Predicted_Data")
tweetsdf <- readRDS("Tweets_Predicted_w-9.rds")
usersdf  <- readRDS("Users_Predicted_w-9.rds")
###################################################################################################
# 3) Appendix Table Hashtag Count
###################################################################################################
library(tidytext)

part0 <- NULL
hashtags <- NULL
j <- 0
code2 <- paste0("part", j, "<- c(part", j, ",vector_hash)")

# Use loop for it instead of base R since Data is to big for base...
for(i in 1:nrow(tweetsdf)){
  if(i %% 10000 == 0){
    j <- j+1
    assign(paste("part", j, sep = ""), NULL) 
    code1 <- paste0("hashtags<- c(hashtags, part", j-1,")")
    eval(parse(text = code1))
    code2 <- paste0("part", j, "<- c(part", j, ",vector_hash)")
    code3 <- paste0("rm(part",j-1,")")
    eval(parse(text = code3))
  }
  
  vector_hash <- unlist(tweetsdf[i,'hashtags'])
  vector_hash <- gsub("[^[:alnum:][:blank:]?&/\\-]", "",vector_hash)
  vector_hash <- paste0("#", vector_hash)
  vector_hash <- unique(vector_hash)
  vector_hash <-paste(vector_hash,collapse=" ") 
  vector_hash <- tolower(vector_hash)
  
  eval(parse(text = code2))
  
  if(i %% 10000 == 0){
    cat(i, " rows have now been processed!\n")
  }
  
}

# Combine Hashtgs with last part of for loop which is part 2853.
hashtags <- c(hashtags, part2853)
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
saveRDS(hashtags, "hashtagsraw.rds")

hashtags <- tibble(hashtags)
hashtagscount <- hashtags %>%
  unnest_tokens(word, hashtags) %>% count(word, sort = TRUE)

hashtaglist <- c("ca10","jeffdenham","joshharder","doNothingangrydenham","defeatdenham","dumpdenham","bayareaharder",
                 "ca25","steveknight","katiehill","gethilltothehill","knightout","voteknightout","voteforknight",
                 "ca39","youngkimcd39","teamyoung","gillcisnerosca","flipocblue","gilonthehill","cisneros4congress",
                 "ca48","danarohrabacher","harleyrouda","teamharley","rohrabacher","voteharley","hireharley","votefordana",
                 "co06","mikecoffman","jasoncrow","teamcrow","votecrow","teamcoffman","votecoffman",
                 "fl16","vernbuchanan","davidShapiro","redtidevern","vernsyacht",
                 "fl18","teambaer","brianmast","laurenbaer","brianmastistoxic","teammast","mastforcongress",
                 "tx07","congculberson","johnculberson","teamlizzie","lizzie4congress","voteoutculberson","voteculberson",
                 "tx32","teamcollin","collinallred","collinallredtx","petesessions","votepetesessions",
                 "tx23","willhurd","ginajones","tx23forgina","gina2018","hurdforcongress","hurdonthehill",
                 "ny19","delgadoforny19","antoniodelgado","johnfasony","teamfaso","firefaso",
                 "ny22","abrindisiny","claudiatenney","brindisiforcongress","onetermtenney","tenneyforcongress",
                 "il12","kellycoalition","repbost","flipil12","votekelly","mikebost",
                 "ks03","kevinyoder","sharicedavids","yodervoter","votedavids",
                 "ks02","stevewatkins","steve4kansas","pauldavis","pauldavisks",
                 "mn01","teamfeehan","danfeehan","danielfeehan","jimhagedornmn","jimhegedorn",
                 "mn08","petestauber","joeradinovich","votestauber","joeradical",
                 "oh12","troybalderson","dannycoonnor","voteoconnor","votebalderson",
                 "oh01","stevechabot","aftabpureval","chabotage","teamaftab","teamchabot","votechabot","voteaftab",
                 "republicans","votered","gop","maga","makeamericagreatagain","#redtsunami",
                 "greatawakening","unhinged","walkawaycampaign","walkaway","republicans2018",
                 "democratspartyofhate","liberalmob","keepitred","#remainred",
                 "democrats","voteblue","bluewave","masa","MakeAmericsSmartAgain","DumpTrump",
                 "demswork4usa","yeswecan","flipitblue","democrats2018","winblue","flipthehouse",
                 "redtoblue", "#takeitback","midtermelection2018","election2018","midterms","electionday","midterms2018",
                 "trump","congressman","congressmen","congresswoman","congersswomen","registertovote","yourvoteisyourvoice")

hashtagscountfin <- hashtagscount %>% arrange(n) %>% filter(word %in% hashtaglist)
saveRDS(hashtagscountfin, "hashtagsfin.rds")
###################################################################################################
# 4) Descriptive Plots
###################################################################################################
## 4.1) Descriptives Dataset before n < 10 removed
###################################################################################################
# Ratio between National and District Tweets
df_type <- tweetsdf %>% group_by(District) %>% summarise(n = n()) %>% mutate(freq = 100*(n / sum(n)))

# Ratio between Republican / Democratic / Neutral Tweets:
df_party <- tweetsdf %>% count(Democrats, Republican) %>% mutate(freq = n / sum(n))

# Bot table (Pipeing some stuff)
countbosts <- tweetsdf %>% summarise(Bots = 100 * sum(predict == 1)/n(), Genuine = 100 * sum(predict == 0)/n(),
                                     BotsDistrict = 100 * sum(predict == 1 & District != "none")/sum(District != "none"),
                                     GenuineDistrict = 100 * sum(predict == 0 & District != "none")/sum(District != "none"),
                                     BotNational = 100 * sum(predict == 1 & National == 1)/sum(National == 1),
                                     GenuineNational = 100 * sum(predict == 0 & National == 1)/sum(National == 1))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
write.csv(countbosts, "Table_Bot_Stats_before_change.csv")
###################################################################################################
## 4.2) Bot Distribution and optimal Thershold for classification
###################################################################################################
### 4.2.1) Prediction Distribution of Tweets from Bots
###################################################################################################
pl_bot <- ggplot(tweetsdf) +
  geom_histogram(aes(y=stat(width*density), x=p0), color = "black", fill = "grey", bins = 100) +
  labs(x = "Predicted Probability of Genuine Tweet", y = "Share [%]", 
       caption="Collection period Oct. 6th to Nov. 14th 2018") +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0),
                     limits = c(0,0.20)) +
  scale_x_continuous(labels = percent_format()) +
  theme_classic(base_size = 16) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"),
        axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("dist_attr_scores_tweets.png", pl_bot, width = 12, height = 6, dpi = 300)
###################################################################################################
### 4.2.2) Prediction Distribution of Users from Bots
###################################################################################################
pl_bot2 <- ggplot(usersdf) +
  geom_histogram(aes(y=stat(width*density), x=p0), color = "black", fill = "grey", bins = 100) +
  labs(x = "Predicted Probability of Genuine User", y = "Share [%]", 
       caption="Collection period Oct. 6th to Nov. 14th 2018") +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0),
                     limits = c(0,0.10)) +
  scale_x_continuous(labels = percent_format()) +
  theme_classic(base_size = 16) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"),
        axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("dist_attr_scores_user.png", pl_bot2, width = 12, height = 6, dpi = 300)
###################################################################################################
### 4.2.3) Analysis of Bot Peak at 0.75 
###################################################################################################
#### 4.2.3.0) Variance Table 
###################################################################################################
count_predict_scores <- tweetsdf %>% group_by(p1) %>% summarise(n = n()) %>% arrange(desc(n)) %>% filter(p1 > 0.7)
count_predict_scores <- head(count_predict_scores, 1)

# Filter all Tweets with the p1 value which is responsible for peak
peak_bot_df <-  usersdf %>% filter(p1 > 0.7178372) %>% filter(p1 < 0.7178374)

# Count number of tweets in data per user and filter them
counttweets <- tweetsdf %>% filter(p1 > 0.7178372) %>% filter(p1 < 0.7178384) %>% group_by(user_id) %>% summarise(tweets_collected = n())

# Plot Distribution of number of Tweezt of these accounts 

pl_peak <- ggplot(counttweets) +
  stat_count(mapping = aes(x=tweets_collected, y=..prop.., group=1), color = "black", fill = "grey") +
  scale_x_continuous(limits = c(0,25),
                     breaks = c(seq(0,25, by = 1)),
                     expand = c(0,0)) +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0)) +
  labs(x = "Number of Tweets per account collected", y = "Percent of accounts", 
       caption = paste0("The maximum number of tweets collected of these accounts is: ", max(counttweets$tweets_collected))) +
  theme_classic(base_size = 16)

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Peak_Dist_Plot.png", pl_peak, width = 12, height = 8, dpi = 300)


# Merge the two df's
peak_bot_df <- merge(peak_bot_df, counttweets, by = "user_id", all.x = T)

# Reinstateimputed NA in Profile_Background....
peak_bot_df$profile_background_default <- ifelse(is.na(peak_bot_df$profile_background_default), 0,1)

# Compute Variance of Columns at this peak!
lapply(peak_bot_df, class)

w <- which(sapply(peak_bot_df, class) == 'impute')
peak_bot_df[w] <- lapply( peak_bot_df[w], function(x) as.numeric(as.character(x)))

peak_bot_df$location <- NULL
peak_bot_df$lang <- NULL
peak_bot_df$screen_name <- NULL
peak_bot_df$predict <- NULL
peak_bot_df$name <- NULL
peak_bot_df$account_lang <- NULL

w2 <- which(sapply(peak_bot_df, class) == 'factor')
peak_bot_df[w2] <- lapply( peak_bot_df[w2], function(x) as.numeric(as.character(x)))

w3 <- which(sapply(peak_bot_df, class) == 'integer')
peak_bot_df[w3] <- lapply( peak_bot_df[w3], function(x) as.numeric(x))

#Rescale all vars from 0 to 1 since this was also done for the DNN
peak_bot_df <- data.frame(lapply(peak_bot_df[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,40)], 
                                 function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/1)))

varpeak <- as.data.frame(resample::colVars(peak_bot_df), na.rm = T)
colnames(varpeak) <- c("Variance")
varpeak$variables <- rownames(varpeak)
rownames(varpeak) <- c(rep(1:nrow(varpeak)))

sdpeak <- as.data.frame(resample::colStdevs(peak_bot_df), na.rm = T)
colnames(sdpeak) <- c("SD")
sdpeak$variables <- rownames(sdpeak)
rownames(sdpeak) <- c(rep(1:nrow(sdpeak)))

varsdpeak <- merge(varpeak, sdpeak, by = "variables")

varnames <- names(peak_bot_df)
j <- 0
typevar <- data.frame()
for(i in varnames){
  j <- j +1
  typevar[j,1] <- paste(i)
  typevar[j,2] <- mean(peak_bot_df[,paste(i)])
  typevar[j,3] <- ifelse(length(unique(peak_bot_df[,paste(i)])) >= 3, "numeric", "binary")
}
colnames(typevar) <- c("variables", "Mean", "Variable Type")

varsdpeak <- merge(varsdpeak, typevar, by = "variables")
varsdpeak <- varsdpeak %>% filter(variables != "tweets_collected")
varsdpeak <- varsdpeak %>% arrange(desc(Variance)) %>% mutate_at(2:4, round, 3)

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
write_csv(varsdpeak, "Variance_of_classification_peak.csv")
###################################################################################################
#### 4.2.3.1) Principal comonent Analysis (Multiple Factor Analysis)
###################################################################################################
# Factorize some variables:
peak_bot_df$profile_background_default <- as.factor(as.character(peak_bot_df$profile_background_default))
peak_bot_df$profile_banner_default <- as.factor(as.character(peak_bot_df$profile_banner_default))
peak_bot_df$profile_image_default <- as.factor(as.character(peak_bot_df$profile_image_default))
peak_bot_df$geo_enabled <- as.factor(as.character(peak_bot_df$geo_enabled))
peak_bot_df$verified <- as.factor(as.character(peak_bot_df$verified))

res.mfa <- MFA(peak_bot_df, group = c(5, 1, 3, 6, 1, 1, 3, 3, 6, 1, 1, 1, 3, 1, 1),
               type = c("s", "n", "n", "s", "n", "s", "s", "s", "s", "s", 
                        "s", "s", "s", "s", "s"),
               name.group = c("User-Statistics", "Verified", "Profile-Design", "Username",
                              "Location", "Account Age", "Text-Sentiment", "Text-Length",
                              "Captions", "Retweet-Ratio", "URL-Ratio", "Reply-Ratio", 
                              "User-Ratios", "Tweets-per-day", "Tweetnumber"),
               graph = FALSE)
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
saveRDS(res.mfa, "MFA-Analysis.rds")

res.mfa <- readRDS("MFA-Analysis.rds")

# Groups of variables 
group <- get_mfa_var(res.mfa, "group")
fviz_mfa_var(res.mfa, "group")


#Quantitative variables 
quanti.var <- get_mfa_var(res.mfa, "quanti.var")


fviz_mfa_var(res.mfa, "quanti.var", palette = "grey", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")
###################################################################################################
### 4.2.4) Table 1 MA Thesis
###################################################################################################
tweetsdf$is_quote <- ifelse(is.na(tweetsdf$quoted_user_id), 0, 1)
tweetsdf$is_retweet_a <- ifelse(tweetsdf$is_quote == 0 & tweetsdf$is_retweet == 1, 1,0)

countcases <- tweetsdf %>% summarise(quotes= sum(is_quote == 1), 
                                     retweets= sum(is_retweet_a == 1), 
                                     replies= sum(is_reply == 1), 
                                     retweetsandquotes= sum(is_retweet == 1))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
write.csv(countcases, "Table_Tweet_Stats_Data_Section.csv")
###################################################################################################
### 4.2.5) Remove all users and their tweets with n less than 10 
###################################################################################################
counttweets <- tweetsdf %>% group_by(user_id) %>% summarise(n = n())

summary(counttweets$n)

n_plot <- ggplot(counttweets) +
            stat_count(aes(y=..prop.., x = n, group=1), color = "black", fill = "grey") +
            labs(x = "Number of collected tweets for a user", y = "Share [%]", 
                 caption = "Collection Period Oct. 6th to Nov. 14th 2018") +
            annotate("text", x = 21, y = .05, label = paste("Maximum number of tweets for a user is:", max(counttweets$n))) +
            scale_y_continuous(labels = percent_format(),
                               expand = c(0,0)) +
            scale_x_continuous(expand = c(0,0),
                               limits = c(0,25)) +
            theme_classic(base_size = 16) +
            theme(panel.background = element_blank(), 
                  panel.grid = element_blank(),
                  panel.border = element_blank(),
                  plot.margin = unit(c(.5,1,.5,1), "cm"),
                  axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Tweets_per_User_dist.png", n_plot, width = 12, height = 8, dpi = 300)

usersdf <- merge(usersdf, counttweets, by = "user_id", all.x = T)
usersdf <- usersdf %>% filter(n > 9)

tweetsdf <- merge(tweetsdf, counttweets, by = "user_id", all.x = T)
tweetsdf <- tweetsdf %>% filter(n > 9)

#New Prediction Distribution
pl_bot3 <- ggplot(usersdf) +
  geom_histogram(aes(y=stat(width*density), x=p0), color = "black", fill = "grey", bins = 100) +
  labs(x = "Predicted Probability of Genuine User with n >= 10", y = "Share [%]", 
       caption="Collection period Oct. 6th to Nov. 14th 2018") +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0),
                     limits = c(0,0.14),
                     breaks = c(.025,.05,.075,.1,.125)) +
  scale_x_continuous(labels = percent_format()) +
  theme_classic(base_size = 16) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"),
        axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("dist_attr_scores_user_final.png", pl_bot3, width = 12, height = 6, dpi = 300)
rm(pl_bot3)

pl_bot4 <- ggplot(tweetsdf) +
  geom_histogram(aes(y=stat(width*density), x=p0), color = "black", fill = "grey", bins = 100) +
  labs(x = "Predicted Probability of Genuine Tweet from users with n >= 10", y = "Share [%]", 
       caption="Collection period Oct. 6th to Nov. 14th 2018") +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0),
                     limits = c(0,0.20),
                     breaks = c(.025,.05,.075,.1,.125,.15,.175,.2)) +
  scale_x_continuous(labels = percent_format()) +
  theme_classic(base_size = 16) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"),
        axis.text.x = element_text(angle = 0, vjust = 1.0, hjust = 1.0))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("dist_attr_scores_tweets_final.png", pl_bot4, width = 12, height = 6, dpi = 300)
rm(pl_bot4)
###################################################################################################
## 4.3) Tweets per Hour
###################################################################################################
# Get Timesensitive Information and visulize it
###################################################################################################
### 4.3.1) Tweets per hour plot by Party (Number of Tweets per hour posted around the US Midterms 2018 by Party affiliation)
###################################################################################################
tweetsdf$created_at_hour <- cut(tweetsdf$created_at, breaks = "hour")
tweets_hour <- tweetsdf %>% group_by(created_at_hour, Democrats, Republican, predict) %>% summarise(n = n()/1000)

tweets_hour$Bot_Not <- NA
tweets_hour$Bot_Not[tweets_hour$predict==1] = "Bot"
tweets_hour$Bot_Not[tweets_hour$predict==0] = "GenuineAccount" 
tweets_hour$Party_favoured <- NA
tweets_hour$Party_favoured <- "notclear"
tweets_hour$Party_favoured[tweets_hour$Republican==1] = "Republican"
tweets_hour$Party_favoured[tweets_hour$Democrats==1] = "Democrats" 
tweets_hour$created_at_hour <- as.POSIXct(tweets_hour$created_at_hour)

facetlabel <- c('notclear' ='No affiliation', 'Republican' = 'Tweets affiliated with the Republican-Party',
                'Democrats' = 'Tweets affiliated with the Democratic-Party')

group.colors <- c(Bot="#d62728", GenuineAccount="#1f77b4")
pl_time <- ggplot(tweets_hour, aes(x = created_at_hour, y = n, color = Bot_Not, fill = Bot_Not)) +
            geom_point(alpha =.5, size =1) +
            geom_line(alpha = .25, size = 1) +
            labs(x = "Date", y = "Number of Tweets [in thousands]", 
                 caption="Collection period Oct. 6th to Nov. 14th 2018", color='Favoured Party', fill='Favoured Party') +
            scale_color_manual(values=group.colors, labels = c("Social Bots", "Genuine Accounts")) +
            scale_fill_manual(values=group.colors, labels = c("Social Bots", "Genuine Accounts")) +
            scale_x_datetime(date_breaks = "24 hour", labels = date_format("%b %d")) +
            scale_y_continuous(expand = c(0,0)) +
            guides(color = guide_legend(override.aes = list(size=2))) +
            facet_wrap(~Party_favoured, ncol = 1, scales = "free", labeller = as_labeller(facetlabel)) +
            theme_classic(base_size = 16) +
            theme(legend.position="bottom", legend.direction="horizontal") +
            theme(panel.background = element_blank(), 
                  panel.grid = element_blank(),
                  panel.border = element_blank(),
                  plot.margin = unit(c(.5,1,.5,1), "cm"),
                  strip.background = element_rect(color="white", fill="white"),
                  strip.text.x = element_text(angle = 0, hjust = 0),
                  strip.text = element_text(size=16),
                  axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))
            

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Tweets_over_time.png", pl_time, width = 12, height =12, dpi = 300)
###################################################################################################
### 4.3.2) Avergae Tweets by Pary Afiliation...
###################################################################################################
# Share of Tweets by Party Affiliation and Account Type
repdemp <- tweetsdf %>% group_by(Republican, Democrats, predict) %>% summarize(n = n())
repdemp$freq <- repdemp$n / sum(repdemp$n)

# Share of Tweets by Party Affiliation (no diff Acc. Type)
repdem <- tweetsdf %>% group_by(Republican, Democrats) %>% summarize(n = n())
repdem$freq <- repdem$n/sum(repdem$n)

# 
tweets_hour$created_at_day <- cut(tweets_hour$created_at_hour, breaks = "day")

tweets_hour_before_elect <- tweets_hour %>% filter(created_at_hour <= as.POSIXct("2018-11-06 10:00:00 CEST"))

meantweets_day <- tweets_hour_before_elect %>% group_by(Party_favoured, Bot_Not, created_at_day) %>% 
  summarize( mean_day = mean(n)*1000) %>% 
  group_by(Party_favoured, Bot_Not) %>% 
  summarize(meanday = mean(mean_day), varday = var(mean_day), sdday = sd(mean_day)) %>% 
  mutate(freq = (meanday / sum(meanday)))



setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
write_csv(meantweets_day,"avg_tweets_per_day_party_bot_before_elect.csv")
###################################################################################################
### 4.3.3) Tweets per hour plot by Tweet Type (Bot or Not)
###################################################################################################
tweets_hour <- tweetsdf %>% group_by(created_at_hour, predict) %>% summarise(n = n()/1000)

tweets_hour$Bot_Not <- NA
tweets_hour$Bot_Not[tweets_hour$predict==1] = "Bot"
tweets_hour$Bot_Not[tweets_hour$predict==0] = "GenuineAccount" 
tweets_hour$created_at_hour <- as.POSIXct(tweets_hour$created_at_hour)

#Label DF
label.df <- data.frame(created_at_hour = as.POSIXct(c("2018-10-06 22:00:00 CEST",
                                                      "2018-10-22 22:00:00 CEST",
                                                      "2018-10-25 22:00:00 CEST",
                                                      "2018-11-06 22:00:00 CEST")),
                       n = c(110, 110, 110, 110),
                       Bot_Not = c("GenuineAccount","GenuineAccount",
                                   "GenuineAccount","GenuineAccount"))

group.colors <- c(Bot = "#d62728", GenuineAccount = "#1f77b4")
pl_time2<- ggplot(tweets_hour, aes(x = created_at_hour, y = n, color = Bot_Not, fill = Bot_Not)) +
  geom_rect(aes(xmin=as.POSIXct("2018-10-06 10:00:00 CEST"),xmax=as.POSIXct("2018-10-07 10:00:00 CEST"),ymin=-Inf,ymax=Inf),
            alpha=0.05,fill="lightgrey",color="white") +
  geom_rect(aes(xmin=as.POSIXct("2018-10-22 10:00:00 CEST"),xmax=as.POSIXct("2018-10-23 10:00:00 CEST"),ymin=-Inf,ymax=Inf),
            alpha=0.05,fill="lightgrey",color="white") +
  geom_rect(aes(xmin=as.POSIXct("2018-10-25 10:00:00 CEST"),xmax=as.POSIXct("2018-10-26 10:00:00 CEST"),ymin=-Inf,ymax=Inf),
            alpha=0.05,fill="lightgrey",color="white") +
  geom_rect(aes(xmin=as.POSIXct("2018-11-06 10:00:00 CEST"),xmax=as.POSIXct("2018-11-07 10:00:00 CEST"),ymin=-Inf,ymax=Inf),
            alpha=0.05,fill="lightgrey",color="white") +
  geom_point(alpha = 0.5, size = 1) +
  geom_line(size = 1, alpha = .25) +
  scale_color_manual(values=group.colors, labels = c("Social Bots", "Genuine Accounts")) +
  scale_fill_manual(values=group.colors, labels = c("Social Bots", "Genuine Accounts")) +
  labs(x = "Date", y = "Number of Tweets [in thousands]", 
       caption="Collection period Oct. 6th to Nov. 14th 2018", color='Tweets made by:', fill='Tweets made by:') +
  scale_x_datetime(date_breaks = "24 hour", labels = date_format("%b %d")) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,115)) +
  guides(color = guide_legend(override.aes = list(size=2))) +
  geom_text(data = label.df, label = "*", color = "black", size = 8) +
  theme_classic(base_size = 16) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Tweets_over_time_bot.png", pl_time2, width = 12, height = 6, dpi = 300)
###################################################################################################
### 4.3.4) Average Tweets by Classification type 
###################################################################################################
tweetsdf$created_at_day <- as.Date(cut(tweetsdf$created_at, breaks = "day"), format = "%Y-%m-%d")
tweets_elect_day <- tweetsdf %>% filter(created_at_day == as.Date("2018-11-06", format = "%Y-%m-%d") | 
                                        created_at_day == as.Date("2018-11-07", format = "%Y-%m-%d")) %>% 
                                 summarise(n = n())


tweets_hour$created_at_day <- cut(tweets_hour$created_at_hour, breaks = "day")

tweets_hour_before_elect <- tweets_hour %>% filter(created_at_hour <= as.POSIXct("2018-11-06 10:00:00 CEST"))

meantwwets_day <- tweets_hour_before_elect %>% group_by(Bot_Not, created_at_day) %>% 
                                  summarize( mean_day = mean(n)*1000) %>% 
                                  group_by(Bot_Not) %>% 
                                  summarize(meanday = mean(mean_day), varday = var(mean_day), sdday = sd(mean_day))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
write_csv(meantwwets_day,"avg_tweets_per_day_before_elect.csv")

tweets_hour_after_elect <- tweets_hour %>% filter(created_at_hour >= as.POSIXct("2018-11-07 10:00:00 CEST"))

meantwwets_day <- tweets_hour_after_elect %>% group_by(Bot_Not, created_at_day) %>% 
  summarize( mean_day = mean(n)*1000) %>% 
  group_by(Bot_Not) %>% 
  summarize(meanday = mean(mean_day), varday = var(mean_day), sdday = sd(mean_day))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
write_csv(meantwwets_day,"avg_tweets_per_day_after_elect.csv")
###################################################################################################
### 4.3.5) Tweets per hour plot by Tweet Type and Party (Bot or Not) Number of Tweets per hour posted around the US Mideterms 2018 by Bots and Genuine Accounts
###################################################################################################
tweets_hour <- tweetsdf %>% group_by(created_at_hour, predict, National) %>% summarise(n = n()/1000)
tweets_hour$Bot_Not <- NA
tweets_hour$Bot_Not[tweets_hour$predict==1 & tweets_hour$National == 1] = "Bot"
tweets_hour$Bot_Not[tweets_hour$predict==1 & tweets_hour$National == 0] = "Bot" 
tweets_hour$Bot_Not[tweets_hour$predict==0 & tweets_hour$National == 1] = "Genuine" 
tweets_hour$Bot_Not[tweets_hour$predict==0 & tweets_hour$National == 0] = "Genuine" 
tweets_hour$Bot_Not <- as.factor(tweets_hour$Bot_Not)
tweets_hour$created_at_hour <- as.POSIXct(tweets_hour$created_at_hour)

facetlabel <- c('0' ='Activity on sampled districts level', '1' = 'Activity on the national level')


group.colors <- c(Bot = "#d62728", Genuine = "#1f77b4")
pl_time3<- ggplot(tweets_hour, aes(x = created_at_hour, y = n, color = Bot_Not, fill = Bot_Not)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_line(size = 1, alpha = .25) +
  scale_color_manual(values=group.colors, labels = c("Social Bots", "Genuine Accounts"),
                     breaks=c("Bot", "Genuine")) +
  scale_fill_manual(values=group.colors, labels = c("Social Bots", "Genuine Accounts"),
                    breaks=c("Bot", "Genuine")) +
  labs(x = "Date", y = "Number of Tweets [in thousands]",
       caption="Collection period Oct. 6th to Nov. 14th 2018", color='Tweets made by:', fill='Tweets made by:') +
  scale_x_datetime(date_breaks = "24 hour", labels = date_format("%b %d")) +
  scale_y_continuous(expand = c(0,0)) +
  guides(color = guide_legend(override.aes = list(size=3))) +
  theme_classic(base_size = 16) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0),
        strip.background = element_rect(color="white", fill="white"),
        strip.text = element_text(size=16),
        strip.text.x = element_text(angle = 0, hjust = 0))+
  facet_wrap(~National, ncol = 1, scales = "free", labeller = as_labeller(facetlabel))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Tweets_over_time_bot_dist_nat.png", pl_time3, width = 12, height = 8, dpi = 300)
###################################################################################################
### 4.3.6) Average Tweets by National/District and type
###################################################################################################
# Share of Tweets by Geo Level and Account Type
distnatbot <- tweetsdf %>% group_by(National, predict) %>% summarize(n = n())
distnatbot$freq <- distnatbot$n / sum(distnatbot$n)

# Share of Tweets by Geo Level
distnat <- tweetsdf %>% group_by(National) %>% summarize(n = n())
distnat$freq <- distnat$n/sum(distnat$n)

# 
tweets_hour$created_at_day <- cut(tweets_hour$created_at_hour, breaks = "day")

tweets_hour_before_elect <- tweets_hour %>% filter(created_at_hour <= as.POSIXct("2018-11-06 10:00:00 CEST"))

meantweets_day <- tweets_hour_before_elect %>% group_by(National, Bot_Not, created_at_day) %>% 
  summarize( mean_day = mean(n)*1000) %>% 
  group_by(National, Bot_Not) %>% 
  summarize(meanday = mean(mean_day), varday = var(mean_day), sdday = sd(mean_day)) %>% 
  mutate(freq = (meanday / sum(meanday)))
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
write_csv(meantweets_day,"avg_tweets_per_day_geo_bot_before_elect.csv")

tweets_hour_after_elect <- tweets_hour %>% filter(created_at_hour >= as.POSIXct("2018-11-07 10:00:00 CEST"))

meantweets_day <- tweets_hour_after_elect %>% group_by(National, Bot_Not, created_at_day) %>% 
  summarize( mean_day = mean(n)*1000) %>% 
  group_by(National, Bot_Not) %>% 
  summarize(meanday = mean(mean_day), varday = var(mean_day), sdday = sd(mean_day)) %>% 
  mutate(freq = (meanday / sum(meanday)))
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
write_csv(meantweets_day,"avg_tweets_per_day_geo_bot_after_elect.csv")

tweets_hour_at_elect <- tweets_hour %>% filter(created_at_hour >= as.POSIXct("2018-11-06 10:00:00 CEST")) %>% 
                                        filter(created_at_hour <= as.POSIXct("2018-11-07 10:00:00 CEST"))

meantweets_day <- tweets_hour_at_elect %>% group_by(National, Bot_Not, created_at_day) %>% 
  summarize( mean_day = mean(n)*1000) %>% 
  group_by(National, Bot_Not) %>% 
  summarize(meanday = mean(mean_day), varday = var(mean_day), sdday = sd(mean_day)) %>% 
  mutate(freq = (meanday / sum(meanday)))
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
write_csv(meantweets_day,"avg_tweets_per_day_geo_bot_at_elect.csv")
###################################################################################################
### 4.3.7) Tweets per hour plot by Tweet Type and Party (Bot or Not) Small Multiple Nice
###################################################################################################
group.colors <- c(NationalBot = "#d62728", DistrictBot = "#ff7f0e", NationalGenuine = "#1f77b4", DistrictGenuine = "#17becf")
pl_time4<- ggplot(tweets_hour, aes(x = created_at_hour, y = n, color = Bot_Not, fill = Bot_Not)) +
  geom_point(alpha = 0.1) +
  geom_smooth(aes(group=Bot_Not), method = 'loess', span = 0.3, se = F, size=0.3) +
  scale_color_manual(values=group.colors, labels = c("Social Bots (National)", "Social Bots (District)", "Genuine Accounts (National)", "Genuine Accounts (Districts)")) +
  scale_fill_manual(values=group.colors, labels = c("Social Bots (National)", "Social Bots (District)", "Genuine Accounts (National)", "Genuine Accounts (Districts)")) +
  labs(title = "Number of Tweets per hour posted around the US Mideterms 2018 by Bots and Genuine Accounts", 
       x = "Date", y = "Number of Tweets [in thousands]", subtitle="Collection period Oct. 6th to Nov. 14th 2018", 
       caption="source: Maël Kubli", color='Tweets made by:', fill='Tweets made by:') +
  scale_x_datetime(date_breaks = "24 hour", labels = date_format("%b %d")) +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0),
                     limits = c(0,0.30)) +
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Tweets_over_time_bot_dist_nat_2.png", pl_time4, width = 12, height = 6, dpi = 300)
###################################################################################################
# 5) Model Features and Metrics
###################################################################################################
# Leave at least one core for your system mabe even two if you want to do other stuff on it. 
h2oServer <- h2o.init(ip="localhost", port=54321, max_mem_size="4g", nthreads=4)

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/GridSearch_Step_2_v2")

model_path <- "D:\\CloudStation\\Universitaet Zuerich\\Master Thesis\\Output\\GridSearch_Step_2_v2\\dl_grid_model_77"
#model_path <- "/Users/Mael/CloudStation/Universitaet Zuerich/Master Thesis/Output/GridSearch_Step_2_v2/dl_grid_model_77"

# Load best Model int H2O
best_model <- h2o.loadModel(model_path)


# Get Test Metrics & Validation Metrics
Test_Metrics <- readRDS("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/GridSearch_Step_2_v2/Metrics_Test_Set_1_dl_grid_model_77.rds")
Validation_Metrics <- readRDS("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/GridSearch_Step_2_v2/Metrics_Validation_Set_1_dl_grid_model_77.rds")


Test_Metrics <- Test_Metrics[["thresholds_and_metric_scores"]]
Validation_Metrics <- Validation_Metrics[["thresholds_and_metric_scores"]]
Test_Metrics <-       Test_Metrics %>% filter(f1 == max(Test_Metrics$f1))
Validation_Metrics <- Validation_Metrics %>% filter(f1 == max(Validation_Metrics$f1))


Variable_Importance <- read_csv("Variable_Importance_dl_grid_model_77.csv")

varplot <- ggplot(Variable_Importance,aes(x=reorder(variable, scaled_importance), y =scaled_importance)) +
  geom_bar(stat="identity", width=0.5, color = "black", fill = "grey",
           position=position_dodge()) +
  geom_text(label="Input Dropoutratio", x= 1.2, y= 0.26, color = "black", size = 5) +
  coord_flip() +
  geom_hline(yintercept = 0.15, color= "red", linetype = "dashed", size = 0.9) +
  labs(x = "Variables", y = "Scaled Importance of Variables") +
  scale_x_discrete(labels = c("# Hashtags (median)", "Verified Account (False)", "Tweets per Day", 
                              "# Digits In Name", "Profile Image Default (False)", "Screen Name Length", 
                              "Name Length", "Profile Image Default (True)", "Word Count (SD)", 
                              "Listed/Statuses", "Word Count (median)", "URL Ratio", "# Hashtags (mean)",
                              "Word Count (mean)", "Listed Tweets Count", "# Digits In Description",
                              "# Hashtags (SD)", "Profile Background Default (False)", "Sentiment (mean)",
                              "# Digits in Screen Name", "# Mentions (median)", "Geotagging Enabled (False)",
                              "# Statuses", "Retweet Ratio", "Reply Ratio", "Sentiment (median)",
                              "Description Length", "Profile Background Default (True)", "# Mentions (SD)",
                              "Verified Account (True)", "# Mentions (mean)", "# Followers", "Geotagging Enabled (True)",
                              "Profile Banner Default (False)", "# Favourites", "# Friends", "Friends/Follwer",
                              "Snetiment (SD)", "Favourites/Statues", "Profile Banner Default (True)", "Account Age")) +
  theme_classic(base_size = 14)

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Variable_Importance_plotr_2.png", varplot, width = 8, height = 12, dpi = 300)

h2o.shutdown(prompt = FALSE)
###################################################################################################
# 6) Descriptive Tables
###################################################################################################
## 6.1) Table 10 MA Thesis
###################################################################################################
tweetsdf$is_quote <- ifelse(is.na(tweetsdf$quoted_user_id), 0, 1)
tweetsdf$is_retweet_a <- ifelse(tweetsdf$is_quote == 0 & tweetsdf$is_retweet == 1, 1,0)

countcases <- tweetsdf %>% summarise(quotes= sum(is_quote == 1), 
                                     retweets= sum(is_retweet_a == 1), 
                                     replies= sum(is_reply == 1), 
                                     retweetsandquotes= sum(is_retweet == 1),
                                     bots = sum(predict == 1),
                                     genuine = sum(predict == 0))

bots <- usersdf %>% summarise(bots_user = sum(predict == 1),
                              genuine_users = sum(predict == 0))

countcases <- cbind(countcases, bots)

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
write.csv(countcases, "Table_Tweet_Stats_Methods_Section.csv")
###################################################################################################
## 6.2) Table XX MA Thesis
###################################################################################################
countbosts <- tweetsdf %>% summarise(Bots = 100 * sum(predict == 1)/n(), Genuine = 100 * sum(predict == 0)/n(),
                                     BotsDistrict = 100 * sum(predict == 1 & District != "none")/sum(District != "none"),
                                     GenuineDistrict = 100 * sum(predict == 0 & District != "none")/sum(District != "none"),
                                     BotNational = 100 * sum(predict == 1 & National == 1)/sum(National == 1),
                                     GenuineNational = 100 * sum(predict == 0 & National == 1)/sum(National == 1))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Tables")
write.csv(countbosts, "Table_Bot_Stats_final.csv")
###################################################################################################