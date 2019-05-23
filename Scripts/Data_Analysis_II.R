###################################################################################################
# Analysis of Data Part II
###################################################################################################
# Description
###################################################################################################
# This Part contains the code to test the hypothesis I 
# You need at least 48 GB of Ram to run the code with the full data.
###################################################################################################
# Content
###################################################################################################
# 1) Dependencies
# 2) Load Data
# 3) Remove Accounts with less than 10 tweets
# 4) Add Election Data to tweets
# 5) Functions 
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
library(cowplot)
library(scales)
library(reshape2)
library(h2o)
library(lubridate)
library(stats)
library(factoextra)
library(FactoMineR)
library(readxl)
library(glm2)
library(aod)
###################################################################################################
# 2) Load Data
###################################################################################################
# Load Tweets Data
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Predicted_Data")
tweetsdf <- readRDS("Tweets_Predicted_w-9.rds")
usersdf  <- readRDS("Users_Predicted_w-9.rds")
# Load US Election Data
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Data on US States")
electdf  <- read_csv("Election_Results_Table_fin.csv")
# Add Closeness of Election
electdf$Closeness <- electdf$Voteshare_Winner - electdf$Voteshare_Loser
electdf$Voteshare_Loser <- NULL
electdf$Voteshare_Winner<- NULL
electdf$Short <- NULL
###################################################################################################
# 3) Remove Accounts with less than 10 tweets
###################################################################################################
counttweets <- tweetsdf %>% group_by(user_id) %>% summarise(n = n())

usersdf <- merge(usersdf, counttweets, by = "user_id", all.x = T)
usersdf <- usersdf %>% filter(n > 9)

tweetsdf <- merge(tweetsdf, counttweets, by = "user_id", all.x = T)
tweetsdf <- tweetsdf %>% filter(n > 9)

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Main_Data/Predicted_Data")

#saveRDS(usersdf, "Users_Predicted_w-9_n10.rds")
#saveRDS(tweetsdf, "Tweets_Predicted_w-9_n10.rds")
usersdf <- readRDS("Users_Predicted_w-9_n10.rds")
tweetsdf <-readRDS("Tweets_Predicted_w-9_n10.rds") 
###################################################################################################
# 4) Add Election Data to tweets
###################################################################################################
electdf$District <- electdf$District_ID
tweetsdf <- merge(tweetsdf, electdf, by = "District", fill = T)
#Clear Memeory
gc()
###################################################################################################
# 5) Functions 
###################################################################################################
# Horowitz Pseudo-R
horo.r2 <- function(nullmod, fullmod){
  ll0 <- as.numeric(logLik(nullmod))
  llv <- as.numeric(logLik(fullmod))
  m <- length(coef(fullmod))-1
  r2 <- 1 -((llv - (m/2)) / (ll0))
  return(r2)
}
###################################################################################################
# 6) Hypothesis Testing
###################################################################################################
## 6.1) H1 
###################################################################################################
### 6.1.1) H1 Models (GLM link = logit)
###################################################################################################
modh1_0 <- glm(predict ~ NULL, data = tweetsdf, family = binomial(link=logit))

modh1_1 <- glm(predict ~ National, data = tweetsdf, family = binomial(link=logit))
summary(modh1_1)

modh1_2 <- glm(predict ~ National + log(Population) , data = tweetsdf, family = binomial(link=logit))
summary(modh1_2)

modh1_3 <- glm(predict ~ National + log(Population) + Bachelor_or_higher , data = tweetsdf, family = binomial(link=logit))
summary(modh1_3)

modh1_4 <- glm(predict ~ National + log(Population) + Bachelor_or_higher + log(Median_Income) , data = tweetsdf, family = binomial(link=logit))
summary(modh1_4)

modh1_5 <- glm(predict ~ National + log(Population) + Bachelor_or_higher + log(Median_Income) + Median_Age , data = tweetsdf, family = binomial(link=logit))
summary(modh1_5)

modh1_6 <- glm(predict ~ National + log(Population) + Bachelor_or_higher + log(Median_Income) + Median_Age + UrbanShare , data = tweetsdf, family = binomial(link=logit))
summary(modh1_6)

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Models")

gc()

#Save Models:
saveRDS(modh1_1, "Simple_Model_H1.rds")
saveRDS(modh1_6, "Full_Model_H1.rds")
###################################################################################################
### 6.1.2) H1 Simulation of Effects (Full Model)
###################################################################################################
#Do Simulation:
set.seed(1991)
gamma.hat <- coef(modh1_6)
V.hat <- vcov(modh1_6)
library(MASS)
S <- mvrnorm(10000,gamma.hat,V.hat)

#Scenario Setup (Values for National Group and Value for District Group (mean))
natdf <- electdf %>% filter(State == "United States")
disdf <- electdf %>% filter(State != "United States")

#Scenario National 
National <- c(1)
PopNat <- c(log(natdf$Population))
EducNat <- c(natdf$Bachelor_or_higher)
IncomeNat <- c(log(natdf$Median_Income))
AgeNat <- c(natdf$Median_Age)
UrbanNat <- c(natdf$UrbanShare)
#Scenario Regional
Regional <- c(0)
PopDist <- c(log(mean(disdf$Population)))
EducDist <- c(mean(disdf$Bachelor_or_higher))
IncomeDist <- c(log(mean(disdf$Median_Income)))
AgeDist <- c(mean(disdf$Median_Age))
UrbanDist <- c(mean(disdf$UrbanShare))

#most likely scenario mode for all other vars
scenario_1 <- cbind(1, National, PopNat, EducNat, IncomeNat, AgeNat, UrbanNat)
scenario_2 <- cbind(1, Regional, PopDist, EducDist, IncomeDist, AgeDist, UrbanDist)

Xbeta1 <- S %*% t(scenario_1)
Xbeta2 <- S %*% t(scenario_2)

p.sim1 <- (exp(Xbeta1))/(1+exp(Xbeta1))
p.sim2 <- (exp(Xbeta2))/(1+exp(Xbeta2))

sim.df <- as.data.frame(rbind(cbind(p.sim1, rep("National", 10000)), cbind(p.sim2, rep("Districts", 10000))), stringsAsFactors = F)
colnames(sim.df) <- c("y", "x")

sim.df$y <- as.numeric(as.character(sim.df$y))

p.mean1 <- apply(p.sim1,2,mean)
p.mean2 <- apply(p.sim2,2,mean)

p.qu1 <- t(apply(p.sim1,2,quantile, prob = c(0.005,0.995)))
p.qu2 <- t(apply(p.sim2,2,quantile, prob = c(0.025,0.975)))

plot.df <- data.frame(x=National, y1=p.mean1, lower1=p.qu1[,1], upper1=p.qu1[,2])
plot.df <- rbind(plot.df, c(Regional, p.mean2, p.qu2[,1], p.qu2[,2]))

plot.df$x <- ifelse(plot.df$x == 1, "National", "Districts")
modh1p1 <- ggplot() +
            geom_jitter(data =  sim.df, aes(x=x, y=y), size = .5, color = "#ff7f0e", width = .2, alpha = .05) +
            geom_errorbar(data = plot.df, aes(x=x, ymin=lower1, ymax=upper1), width=.5,
                          position=position_dodge(0.05), alpha = .75, color = "grey10") +
            geom_point(data = plot.df,aes(x=x, y=y1), color = "black", size = 2) +
            labs(x = "", y = "Share of tweets made by social bots [%]") +
            scale_color_d3() +
            scale_fill_d3() +
            scale_y_continuous(labels = percent_format(),
                               expand = c(0,0),
                               limits = c(.045,.07)) +
            theme_classic(base_size = 16) +
            theme(panel.background = element_blank(), 
                  panel.grid = element_blank(),
                  panel.border = element_blank(),
                  plot.margin = unit(c(.5,1,.5,1), "cm"),
                  axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Model_H1_Signif.png", modh1p1, width = 4, height = 6, dpi = 300)
###################################################################################################
#### 6.1.3) Dist Plot:
###################################################################################################
#Make weight for each District according to size of case with n as weight:
Weight <- tweetsdf %>% group_by(District) %>% summarise(W = n())

# Make DF with values form the three important non binary vars (long format needed):
botrate <- tweetsdf %>% dplyr::select(District_ID, predict) %>% group_by(District_ID) %>% summarize(avgbotrate = mean(as.numeric(as.character(predict)))*100)
plot_dist_df <- electdf %>% dplyr::select(Median_Income, Median_Age, Population, Bachelor_or_higher, UrbanShare)
plot_dist_df$BotRate <- botrate$avgbotrate
plot_dist_df$Median_Income <- log(plot_dist_df$Median_Income)
plot_dist_df$Population <- log(plot_dist_df$Population)
plot_dist_df$District <- electdf$District_ID
plot_dist_df$National <- ifelse(electdf$District_ID == "none", "National", "Districts")
plot_dist_df$Weight <- Weight$W
plot_dist_df <- tidyr::gather(plot_dist_df, Variable, Value, Median_Income:BotRate, factor_key = T)

# Change Order of Boxplot Factor levels:
plot_dist_df$Variable <- relevel(plot_dist_df$Variable, "National")
plot_dist_df$Variable <- relevel(plot_dist_df$Variable, "BotRate")

# Label Titles
facet_titles <- c("National" = "Inependent Variable: National vs Distrcits",
                  "Median_Income" = "Covariate Variable: Median Income",
                  "Median_Age" = "Covariate Variable: Median Age",
                  "Population" = "Covariate Variable: Population",
                  "Bachelor_or_higher" = "Share of Population with Bachelor or higher",
                  "BotRate" = "Dependent Variable: Share of Tweets made by Social Bots")

# Outlier Labeling Functionwith weights:
is_outlier <- function(x, y=1) {
  #Incorparate Weight
  h <- rep(x, y)
  
  return(x < quantile(h, 0.25) - 1.5 * IQR(h) | x > quantile(h, 0.75) + 1.5 * IQR(h))
}

# Use Weights (Instead of making the Boxplots over the big data frame I use a small data frame with each Districts aggreagted values and its weight)
plot_dist_df <- plot_dist_df %>% group_by(Variable) %>% mutate(is_outlier=ifelse(is_outlier(Value, Weight), District, as.character(NA)))



dist_plot <- plot_dist_df %>%
  ggplot(., aes(x = Variable, y=Value, color = Variable, fill = Variable, weight = Weight)) +
  geom_boxplot(alpha = .5, notch = F) +
  scale_x_discrete(labels=c("National" = "Level of Tweet",
                            "BotRate" = "Social Bot [%]",
                            "Median_Income" = "log(Medain Spending) [US$]",
                            "Median_Age" = "Median Age [y]",
                            "Population" = "log(Population)",
                            "Bachelor_or_higher" = "Share of Pop. with Bachelor or higher [%]")) +
  coord_flip() +
  facet_wrap(~Variable, ncol = 1, scales = "free", labeller = as_labeller(facet_titles)) +
  scale_color_d3() +
  scale_fill_d3() +
  theme_minimal(base_size = 14) +
  labs(x = "", title = "Distribution of dependent variable and non binary independent variables in the sampled districts:") +
  theme(legend.position="none", legend.direction="horizontal") +
  theme(axis.line= element_line(),
        panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"),
        axis.text.y = element_text(angle = 45, vjust = 0.0, hjust = 0.5))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Model_H1_dist_bot.png", dist_plot, width = 12 , height = 6, dpi = 300)

###################################################################################################
#### 6.1.4) Combine Plots:
###################################################################################################
alltg <- plot_grid(modh1p1,dist_plot, align = "v", ncol = 2)
alltg

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Model_H1_with_bot_dist.png", alltg, width = 10, height = 6, dpi = 300)
###################################################################################################
### 6.1.5) H1 Hyothesis Testing
###################################################################################################
# Loglikelihood ratio test 
ll1 <- anova(object = modh1_1, test = "Chisq")
ll6 <- anova(object = modh1_6, test = "Chisq")

ll6

#Plot Table of Models
stargazer(modh1_1, modh1_2, modh1_3, modh1_4, modh1_5, modh1_6, type = "html",
          header = F, single.row = F, out = "H1-Models.html", 
          title = "Logit Analyses of determinantes of soical bots, midterm election in the USA 2018",
          dep.var.labels = c("Tweets made by Social Bots"), digits = 3, digits.extra = 4, intercept.bottom = F,
          covariate.labels = c("Constant", "National Tweet", "log(Population)", "Education", "log((Median Income))",
                               "Median Age", "Urban Population Share"), keep.stat=c("n", "ll", "AIC"),
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"))

#Wald Test (chisq)
wt6 <- wald.test(b = coef(modh1_6), Sigma = vcov(modh1_6), Terms = 2)
wt6
saveRDS(w6, "WaldTest_H1.rds")

#LL Test Output
stargazer(ll6, type = "html", out = "H1-Model-Full-LLTest.html", summary = FALSE, digits = 1, digits.extra = 4,
          column.labels = c("Df", "Deviance", "Resid. Df", "Resid. Dev", "Pr(>Chi)"))

## odds ratios only
exp(coef(modh1_6))

## Pseudo R (Horowitz 1982)
horo.r2(modh1_0, modh1_6)

# Keep full and simple model:
rm(modh1_1,modh1_2, modh1_3, modh1_4, modh1_5)
