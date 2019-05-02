###################################################################################################
# Analysis of Data Part III
###################################################################################################
# Description
###################################################################################################
# This Part contains the code to test the hypothesis II
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
library(corrgram)
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
#usersdf <- readRDS("Users_Predicted_w-9_n10.rds")
tweetsdf <-readRDS("Tweets_Predicted_w-9_n10.rds") 
###################################################################################################
# 4) Add Election Data to tweets
###################################################################################################
electdf$District <- electdf$District_ID
tweetsdf <- merge(tweetsdf, electdf, by = "District", fill = T)
#Clear Memeory
gc()

# Filter only tweets from districts
tweetsdf <- tweetsdf %>% filter(National == 0)

# PVI Index make all values Positive otherwiese the glm will not work as there should be less activity when far away from zero and higher activity when close to zero!
tweetsdf$PVI_Index_NUM <- abs(tweetsdf$PVI_Index_NUM)
tweetsdf$Cook_Partisan_Voting_Index <- as.factor(tweetsdf$Cook_Partisan_Voting_Index)
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
## 6.1) H2
###################################################################################################
## 6.1.1) H2 Principal Component Analysis
###################################################################################################
# Testing if independent variables are measuring as well as with normal models
tweetsdf_mod <- tweetsdf %>% select(predict, Closeness, PVI_Index_NUM)

# Transform all categorical Vairables to numerical flag variables!
tweetsdf_mod <- data.frame(model.matrix( ~ . -1, data = tweetsdf_mod))
tweetsdf_mod$predict <- tweetsdf_mod$predict1
tweetsdf_mod$predict0 <- NULL
tweetsdf_mod$predict1 <- NULL

# Lets see how many variables are actually suited to predict y 

cor_tweetsdf_mod <- cor(tweetsdf_mod, tweetsdf_mod, method = "pearson")
cor_df <- data.frame(cor=cor_tweetsdf_mod[2:3,1], varn = names(cor_tweetsdf_mod[2:3,1]))
cor_df <- cor_df %>% mutate(cor_abs = abs(cor)) %>% arrange(desc(cor_abs))
plot(cor_df$cor_abs, type = "l")

# Now lets see what the glm looks like with al varibles without using pcr: 
modh2_all <- glm(predict ~ ., data = tweetsdf_mod, family = binomial(link=logit))
summary(modh2_all)

#PCR 
xv <- tweetsdf_mod %>% select(-predict)
pca <- prcomp(xv, scale. = T, center = T)
plot(pca, type = "l")

# Summary of PCA for Interpretation:
summary(pca)
# The first principle components explain 60 % of the variability of the data. Based on 
# this I remove the second component. 

pca_df <- data.frame(pca$x)
pca_df <- pca_df %>% select(-PC2)
pca_df$predict <- tweetsdf_mod$predict

corrgram(pca_df,lower.panel=panel.cor,upper.panel=panel.pie)

modelh2_pca <- glm(data = pca_df, predict ~ ., family = binomial(link = logit))
summary(modelh2_pca)


# In essence the differences are really small and it is easier to make models as well without using 
# principal comonent analysis. 
###################################################################################################
### 6.1.2) H2 Constructed Variables
###################################################################################################
## ADD Intensity of Campaign Variable (PCA Variable from Closeness and PVI Index)
tweetsdf$Intensity <- pca_df$PC1
## Contestedness Construct
tweetsdf$ContestInt <- tweetsdf$Closeness + (-tweetsdf$PVI_Index_NUM)
###################################################################################################
### 6.1.2) H2 Models (GLM link = logit)
###################################################################################################  Closeness + PVI_Index_NUM
# Models for H2
modh2_0 <- glm(predict ~ NULL, data = tweetsdf, family = binomial(link=logit))
summary(modh2_0)

modh2_1 <- glm(predict ~ Intensity, data = tweetsdf, family = binomial(link=logit))
summary(modh2_1)

modh2_2 <- glm(predict ~ Intensity + Incumbent, data = tweetsdf, family = binomial(link=logit))
summary(modh2_2)

modh2_3 <- glm(predict ~ Intensity + Incumbent + Statewide_Voter_Turnout , data = tweetsdf, family = binomial(link=logit))
summary(modh2_3)

modh2_4 <- glm(predict ~ Intensity + Incumbent + Statewide_Voter_Turnout + log(Total_Spending), data = tweetsdf, family = binomial(link=logit))
summary(modh2_4)

modh2_5 <- glm(predict ~ Intensity + Incumbent + Statewide_Voter_Turnout + log(Total_Spending) + log(Median_Income), data = tweetsdf, family = binomial(link=logit))
summary(modh2_5)

modh2_6 <- glm(predict ~ Intensity + Incumbent + Statewide_Voter_Turnout + log(Total_Spending) + log(Median_Income) + Bachelor_or_higher, data = tweetsdf, family = binomial(link=logit))
summary(modh2_6)

modh2_7 <- glm(predict ~ Intensity + Incumbent + Statewide_Voter_Turnout + log(Total_Spending) + log(Median_Income) + Bachelor_or_higher + Median_Age, data = tweetsdf, family = binomial(link=logit))
summary(modh2_7)

modh2_8 <- glm(predict ~ Intensity + Incumbent + Statewide_Voter_Turnout + log(Total_Spending) + log(Median_Income) + Bachelor_or_higher + Median_Age + UrbanShare, data = tweetsdf, family = binomial(link=logit))
summary(modh2_8)


#Separtated Closeness and PVI Variable:

modh2_18 <- glm(predict ~ Closeness + PVI_Index_NUM + Incumbent + Statewide_Voter_Turnout + log(Total_Spending) + log(Median_Income) + Bachelor_or_higher + Median_Age + UrbanShare, data = tweetsdf, family = binomial(link=logit))
summary(modh2_18)

modh2_11 <- glm(predict ~ Closeness + PVI_Index_NUM, data = tweetsdf, family = binomial(link=logit))
summary(modh2_11)

modh2_12 <- glm(predict ~ Closeness + PVI_Index_NUM + Incumbent, data = tweetsdf, family = binomial(link=logit))
summary(modh2_12)

modh2_13 <- glm(predict ~ Closeness + PVI_Index_NUM + Incumbent + Statewide_Voter_Turnout, data = tweetsdf, family = binomial(link=logit))
summary(modh2_13)

gc()
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Models")
###################################################################################################
### 6.1.3) H2 Hyothesis Testing
###################################################################################################
# Loglikelihood ratio test 
ll18 <- anova(object = modh2_18, test = "Chisq")
ll8 <- anova(object = modh2_8, test = "Chisq")

ll18
ll8

# Plot Table 
stargazer(modh2_1, modh2_2, modh2_3, modh2_4, modh2_8, type = "html",
          header = F, single.row = F, 
          title = "Probit Analyses of Determinantes of Soical Bots, Midtermelection in the USA 2018",
          dep.var.labels = c("Tweets made by Social Bots"), digits = 3, digits.extra = 4, intercept.bottom = F,
          covariate.labels = c("Constant", "Intensity", "Incumbent", "Turnout (State)", "log(Total Spending)",
                              "log(Median Income)", "Education", "Median Age", "Urban Population Share"),
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          out = "H2-Models.html", keep.stat=c("n", "ll", "AIC"))

# WALD TEST 
wt8 <- wald.test(b = coef(modh2_8), Sigma = vcov(modh2_8), Terms = 2:6)
wt8

wt18 <- wald.test(b = coef(modh2_18), Sigma = vcov(modh2_18), Terms = 2:6)
wt18

saveRDS(wt8, "WaldTest_H2.rds")

#LL Test Output
stargazer(ll8, type = "html", out = "H2-Model-Full-LLTest.html", summary = FALSE, digits = 1, digits.extra = 4,
          column.labels = c("Df", "Deviance", "Resid. Df", "Resid. Dev", "Pr(>Chi)"))

## odds ratios only
exp(coef(modh2_8))

## Pseudo R (Horowitz 1982)
horo.r2(modh2_0, modh2_8)

# Keep full and simple model:
rm(modh2_2, modh2_3, modh2_4, modh2_5, modh2_6, modh2_7)

#Save Models:
saveRDS(modh2_1, "Simple_Model_H2.rds")
saveRDS(modh2_8, "Full_Model_H2.rds")
###################################################################################################
### 6.1.4) H2 Simulation of Effects (Full Model)
###################################################################################################
# Monte Carlo Simulation:
## Simulation Base code:
set.seed(1991)
gamma.hat <- coef(modh2_8)
V.hat <- vcov(modh2_8)
library(MASS)
S <- mvrnorm(10000,gamma.hat,V.hat)


## Scenario Setup (Values for National Group and Value for District Group (mean))
disdf <- electdf %>% filter(State != "United States")

#Scenario vectors:
Intensity <- c(seq(-2,2,by = .25))
Incumbent <- 1
Vote_Turn <-c(mean(tweetsdf$Statewide_Voter_Turnout))
Spend <- c(mean(log(tweetsdf$Total_Spending)))
EducDist <- c(mean(disdf$Bachelor_or_higher))
IncomeDist <- c(log(mean(disdf$Median_Income)))
AgeDist <- c(mean(disdf$Median_Age))
UrbanDist <- c(mean(disdf$UrbanShare))
###################################################################################################
#### 6.1.4.1) Incumbent no Incumbent Plot
###################################################################################################
#most likely scenario mean for all other vars
scenario_1 <- cbind(1, Intensity, 0, Vote_Turn, Spend, IncomeDist, EducDist, AgeDist, UrbanDist)
scenario_2 <- cbind(1, Intensity, 1, Vote_Turn, Spend, IncomeDist, EducDist, AgeDist, UrbanDist)

Xbeta1 <- S %*% t(scenario_1)
Xbeta2 <- S %*% t(scenario_2)

p.sim1 <- (exp(Xbeta1))/(1+exp(Xbeta1))
p.sim2 <- (exp(Xbeta2))/(1+exp(Xbeta2))

p.mean1 <- apply(p.sim1,2,mean)
p.mean2 <- apply(p.sim2,2,mean)

p.qu1 <- t(apply(p.sim1,2,quantile, prob = c(0.025,0.975)))
p.qu2 <- t(apply(p.sim2,2,quantile, prob = c(0.025,0.975)))

plot.df <- data.frame(x=Intensity, y1=p.mean1, lower1=p.qu1[,1], upper1=p.qu1[,2], Incumbent = "No")
plot.df2<- data.frame(x=Intensity, y1=p.mean2, lower1=p.qu2[,1], upper1=p.qu2[,2], Incumbent = "Yes")

plot.df <- rbind(plot.df, plot.df2)

modh2p1 <- ggplot() +
  geom_ribbon(data = plot.df, aes(x=x, ymin = lower1, ymax = upper1, fill = Incumbent), 
              alpha = .3, color = NA) +
  geom_smooth(data = plot.df,aes(x=x, y=y1, color = Incumbent), size = 1, method = "loess") +
  labs(x = "Campaign Intensity", y = "Share of tweets made by social bots [%]") +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-2,2)) +
  scale_color_d3() +
  scale_fill_d3() +
  theme_classic(base_size = 14) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"),
        axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Model_H2_Signif_Incumbent.png", modh2p1, width = 6, height = 4, dpi = 300)

###################################################################################################
#### 6.1.4.2) Spending Plot
###################################################################################################
scenario_3 <- cbind(1, Intensity, Incumbent, Vote_Turn, log(quantile(disdf$Total_Spending, .25, na.rm = T)), IncomeDist, EducDist, AgeDist, UrbanDist)
scenario_4 <- cbind(1, Intensity, Incumbent, Vote_Turn, log(quantile(disdf$Total_Spending, .75, na.rm = T)), IncomeDist, EducDist, AgeDist, UrbanDist)

Xbeta3 <- S %*% t(scenario_3)
Xbeta4 <- S %*% t(scenario_4)

p.sim3 <- (exp(Xbeta3))/(1+exp(Xbeta3))
p.sim4 <- (exp(Xbeta4))/(1+exp(Xbeta4))

p.mean3 <- apply(p.sim3,2,mean)
p.mean4 <- apply(p.sim4,2,mean)

p.qu3 <- t(apply(p.sim3,2,quantile, prob = c(0.025,0.975)))
p.qu4 <- t(apply(p.sim4,2,quantile, prob = c(0.025,0.975)))

plot.df3 <- data.frame(x=Intensity, y1=p.mean3, lower1=p.qu3[,1], upper1=p.qu3[,2], Spending = "1st Quartile")
plot.df4<- data.frame(x=Intensity, y1=p.mean4, lower1=p.qu4[,1], upper1=p.qu4[,2], Spending = "3rd Quartile")

plot.df3 <- rbind(plot.df3, plot.df4)

modh2p2 <- ggplot() +
  geom_ribbon(data = plot.df3, aes(x=x, ymin = lower1, ymax = upper1, fill = Spending), 
              alpha = .3, color = NA) +
  geom_smooth(data = plot.df3,aes(x=x, y=y1, color = Spending), size = 1, method = "loess") +
  labs(x = "Campaign Intensity", y = "Share of tweets made by social bots [%]") +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-2,2)) +
  scale_color_d3() +
  scale_fill_d3() +
  theme_classic(base_size = 14) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"),
        axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Model_H2_Signif_Spending.png", modh2p2, width = 6, height = 4, dpi = 300)

###################################################################################################
#### 6.1.4.3) Turnout Plot
###################################################################################################
scenario_5 <- cbind(1, Intensity, Incumbent, quantile(disdf$Statewide_Voter_Turnout, .25, na.rm = T), Spend, IncomeDist, EducDist, AgeDist, UrbanDist)
scenario_6 <- cbind(1, Intensity, Incumbent, quantile(disdf$Statewide_Voter_Turnout, .75, na.rm = T), Spend, IncomeDist, EducDist, AgeDist, UrbanDist)


Xbeta5 <- S %*% t(scenario_5)
Xbeta6 <- S %*% t(scenario_6)

p.sim5 <- (exp(Xbeta5))/(1+exp(Xbeta5))
p.sim6 <- (exp(Xbeta6))/(1+exp(Xbeta6))

p.mean5 <- apply(p.sim5,2,mean)
p.mean6 <- apply(p.sim6,2,mean)

p.qu5 <- t(apply(p.sim5,2,quantile, prob = c(0.025,0.975)))
p.qu6 <- t(apply(p.sim6,2,quantile, prob = c(0.025,0.975)))

plot.df5 <- data.frame(x=Intensity, y1=p.mean5, lower1=p.qu5[,1], upper1=p.qu5[,2], Turnout = "1st Quartile")
plot.df6 <- data.frame(x=Intensity, y1=p.mean6, lower1=p.qu6[,1], upper1=p.qu6[,2], Turnout = "3rd Quartile")

plot.df5 <- rbind(plot.df5, plot.df6)

modh2p3 <- ggplot() +
  geom_ribbon(data = plot.df5, aes(x=x, ymin = lower1, ymax = upper1, fill = Turnout), 
              alpha = .3, color = NA) +
  geom_smooth(data = plot.df5,aes(x=x, y=y1, color = Turnout), size = 1, method = "loess") +
  labs(x = "Campaign Intensity", y = "Share of tweets made by social bots [%]") +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-2,2)) +
  scale_color_d3() +
  scale_fill_d3() +
  theme_classic(base_size = 14) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(.5,1,.5,1), "cm"),
        axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5))

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Model_H2_Signif_Turnout.png", modh2p3, width = 6, height = 4, dpi = 300)
###################################################################################################
#### 6.1.4.3) Dist Plot:
###################################################################################################
#Make weight for each District according to size of case with n as weight:
Weight <- tweetsdf %>% group_by(District) %>% summarise(W = n())

# Make DF with values form the three important non binary vars (long format needed):
pca_var <- unique(tweetsdf$Intensity)
botrate <- tweetsdf %>% dplyr::select(District, predict) %>% group_by(District) %>% summarize(avgbotrate = mean(as.numeric(as.character(predict)))*100)
plot_dist_df <- disdf %>% dplyr::select(Statewide_Voter_Turnout, Total_Spending)
plot_dist_df$Intensity <- pca_var
plot_dist_df$BotRate <- botrate$avgbotrate
plot_dist_df$Total_Spending <- log(plot_dist_df$Total_Spending)
plot_dist_df$District <- disdf$District
plot_dist_df$Weight <- Weight$W
plot_dist_df <- tidyr::gather(plot_dist_df, Variable, Value, Statewide_Voter_Turnout:BotRate, factor_key = T)

# Change Order of Boxplot Factor levels:
plot_dist_df$Variable <- relevel(plot_dist_df$Variable, "Intensity")
plot_dist_df$Variable <- relevel(plot_dist_df$Variable, "BotRate")

# Label Titles
facet_titles <- c("Intensity" = "Inependent Variable: Campaign Intensity",
                  "Total_Spending" = "Independent Variable: Camapaign Spendings by Parties",
                  "Statewide_Voter_Turnout" = "Independent Variable: Voter Turnout",
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
  ggplot(., aes(x = Variable, y=Value, color = Variable, fill = Variable, weight = Weight, label = District)) +
  geom_boxplot(alpha = .5, notch = F) +
  scale_x_discrete(labels=c("Intensity" = "Campaigning Intensity",
                            "BotRate" = "Social Bot [%]",
                            "Total_Spending" = "log(Spending) [US$]",
                            "Statewide_Voter_Turnout" = "Voter Turnout [%]")) +
  geom_text_repel(aes(label=is_outlier),na.rm=TRUE, point.padding = .2) +
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
ggsave("Model_H2_dist_vars.png", dist_plot, width = 12, height = 5, dpi = 300)
###################################################################################################
#### 6.1.4.4) Combine Plots:
###################################################################################################
alltg <- plot_grid(modh2p1,modh2p2,modh2p3, align = "h", ncol = 3)
alltg <- plot_grid(alltg, dist_plot, ncol = 1)
alltg

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/Plots")
ggsave("Model_H2_all_Scenarios.png", alltg, width = 12, height = 10, dpi = 300)
###################################################################################################