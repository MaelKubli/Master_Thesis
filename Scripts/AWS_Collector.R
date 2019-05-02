##########################################################################################
# Twitter Stream of MidtermElections 2018
##########################################################################################
# Content
##########################################################################################
# This Script contains all the neccessary code to stream tweets made during the midterm 
# election campaign. This script works best on a small cloud server, like an Amazon AWS 
# EC2 unit of the size t2.small or t2.medium (1-2 VCPU Cores with 2GB RAM). Smaller 
# instances might work as well but then one has to do without RStudio and run R from the 
# terminal. If you are Interested in how to set up an AWS instance with RStudio see: 
# "https://aws.amazon.com/de/blogs/big-data/running-r-on-aws/"
##########################################################################################
# Contents
##########################################################################################
# 1) Preparations
# 2) Dependencies
# 3) Authentification on Twitter 
# 4) Define hashtags to stream
# 5) Stream Twitter Data
##########################################################################################
# 1) Preparations
##########################################################################################
rm(list=ls(all=T)) # cleans workspace
setwd("~/Actual_Midtermelections") #set dir
options(stringsAsFactors = F) # get rid of factors
##########################################################################################
# 2) Dependencies
##########################################################################################
library(rtweet)
library(jsonlite)
library(rjson)
library(httr)
library(RCurl)
library(data.table)
library(readr)
library(plyr)
library(dplyr)
library(parallel)
###########################################################################################
# 3) Authentification on Twitter 
###########################################################################################
# Loads Twitter Developer App Key
token <- readRDS("~/Actual_Midtermelections/twitter_token_list.rds")
#Gets Rate Limits from twitters API
r <- lapply(token, rate_limit, "lists/statuses")
##########################################################################################
# 4) Define Keywords 
##########################################################################################
keywords <- c("#ElectionDay,#Midterms2018,#MidtermElection2018,#Election2018,#MidTerms,#Democrats,#VoteBlue,#Bluewave,#MASA,#MakeAmericaSmartAgain,#DumpTrump,#DemsWork4USA,#YesWeCan,#FlipItBlue,#Democrats2018,#WinBlue,#Republicans,#VoteRed,#GOP,#MAGA,#MakeAmericaGreatAgain,#RedTsunami,#GreatAwakening,#unhinged,#WalkawayCampaign,#WalkAway,#Republicans2018,#democratspartyofhate,#Trump,#Congressman,#Congresswoman,#Congressmen,#Congresswomen,#RegisterToVote,#YourVoteIsYourVoice,#Vote,#TakeitBack,#FlipTheHouse,#LiberalMob,#CA10,#JeffDenham,#JoshHarder,#DoNothingAngryDenham,#DefeatDenham,#DumpDenham,#BayAreaHarder,#CA25,#SteveKnight,#KatieHill,#gethilltothehill,#knightout,#VoteKnightOut,#VoteForKnight,#CA39,#TeamYoung,#YoungKimCD39,#FlipOCBlue,#GilontheHill,#GilCisnerosCA,#cisneros4congress,#CA48,#DanaRohrabacher,#HarleyRouda,#TeamHarley,#Rohrabacher,#VoteHarley,#HireHarley,#VoteForDana,#CO06,#MikeCoffman,#JasonCrow,#TeamCrow,#VoteCrow,#TeamCoffman,#VoteCoffman,#FL16,#VernBuchanan,#DavidShapiro,#redtidevern,#VernsYacht,#FL18,#TeamBaer,#BrianMast,#laurenbaer,#BrianMastIsToxic,#TeamMast,#MastforCongress,#TX07,#CongCulberson,#JohnCulberson,#TeamLizzie,#Lizzie4Congress,#VoteOutCulberson,#VoteCulberson,#TX32,#TeamCollin,#collinallred,#PeteSessions,#collinallredTX,#VotePeteSessions,#TX23,#WillHurd,#GinaJones,#TX23forGina,#Gina2018,#HurdforCongress,#hurdonthehill,#NY19,#DelgadoforNY19,#JohnFasoNY,#JohnFaso,#FireFaso,#AntonioDelgado,#NY22,#ABrindisiNY,#ClaudiaTenney,#BrindisiforCongress,#onetermtenney,#TenneyforCongress,#IL12,#KellyCoalition,#VoteKelly,#RepBost,#FlipIL12,#KS03,#KevinYoder,#YoderVoter,#VoteDavids,#ShariceDavids,#KeepItRed,#KS02,#PaulDavis,#PaulDavisKS,#SteveWatkins,#RemainRed,#Steve4Kansas,#MN01,#TeamFeehan,#DanFeehan,#JimHagedornMN,#JimHagedorn,#MN08,#PeteStauber,#VoteSatuber,#JoeRadical,#JoeRadinovich,#OH12,#TroyBalderson,#DannyOConnor,#VoteOConnor,#VoteBalderson,#OH01,#SteveChabot,#Chabotage,#AftabPureval,#TeamAftab,#VoteAftab,#TeamChabot,#VoteChabot,#RedToBlue")
##########################################################################################
# 5) Stream Twitter Data
##########################################################################################
# Streaming Time: hours * minutes * seconds
# Actual Stream will be 5 Weeks (four before and one after the midterm elections), which 
# sums to 35 days
streamtime <- 4*60*60 # I stream for 4 hours straight at a time before starting a new data frame
icount <- 6*40 # 6 * number of days stream should run thus 6 files per day i is 6 times days
icount

# Helping Variable for json file identification
daylist <- rep(seq(1,75,by=1), each = 6)
part <- rep(seq(1,6,by=1),times=75)

# Daylight saving Boolean
daysave <- 0

# Stream Tweets to large json file (one file for every 6 hours)
# i runs up to 360 in case there is a reconnect the loop does not stop to early
for(i in 1:360){
  day <- daylist[i]
  partday <- part[i]
  cat("Currently ", i, " steps have been streamed! ", "Day Var is: ", day, " and partday is: ", partday, "\n")
  stream_tweets(q = keywords, timeout = 14400, parse = FALSE, verbose = FALSE,
                file_name = paste0("Midterm_Stream_",day,"part_",partday, ".json"), token = token[[1]])
  cat("One Quarter Day has been Streamed...\n")
  date <- Sys.Date()
  # Day light Saving Exception
  if(date == "2018-10-28" & daysave == 0){
    Sys.sleep(1)
    stream_tweets(q = keywords, timeout = 3600, parse = FALSE, verbose = FALSE,
                  file_name = paste0("Midterm_Stream_Time_Shift.json"), token = token[[1]])
    cat("Daylight saving has been compensated for!\n")
    # Makes sure this is only executed once
    daysave <- 1
  }
  Sys.sleep(1)
}
##########################################################################################
