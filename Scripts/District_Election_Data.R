################################################################################################
# Election Results and District Data
################################################################################################
# 
################################################################################################
# Content:
################################################################################################
# 1) Dependencies
# 2) Load Sepnding Data from Federal Election Commission 
# 3) Load Results and District Data 
################################################################################################
# 1) Dependencies
################################################################################################
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stats)
library(lubridate)
library(stringr)
library(stringi)

# Functions
lowersoph <- function(s, strict = T) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

################################################################################################
# 2) Load Sepnding Data from Federal Election Commission 
################################################################################################
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Data on US States")

spend_df <- read_csv("totals-2019-03-15T10_47_37.csv")
names(spend_df)

#District ID:
spend_df$District_ID <- paste0(spend_df$state,spend_df$district_number)


#Candidate Name
spend_df$Last_Name <- lowersoph(gsub(",.*", "", spend_df$name))
spend_df$First_Name<- lowersoph(gsub(".*? (.+)", "\\1", spend_df$name))
spend_df$First_Name <- gsub(" .*","", spend_df$First_Name)
spend_df$name <- paste(spend_df$First_Name, spend_df$Last_Name)
################################################################################################
# 3) Load Results and District Data 
################################################################################################
# Offical Results by States and Data to the Districts from the census Bureau (numbers adjusted to 2018)
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Data on US States")

ele_df <- readxl::read_xlsx("Election_Results_Table.xlsx")
names(ele_df)
################################################################################################
# 4) Filter Data
################################################################################################
# Filter the 19 Districts
spend_df <- spend_df %>% filter(District_ID %in% unique(paste0(ele_df$Short,ele_df$District_Number)))

# Filter Candidates 
lastnames <- c("Denham", "Knight", "Kim", "Coffman", "Buchanan", 
               "Mast", "Culberson", "Hurd", "Sessions", "Faso", 
               "Tenney", "Bost", "Watkins", "Yoder", "Hagedorn", 
               "Stauber", "Chabot", "Balderson", "Rohrabacher",
               "Rouda", "Harder", "Hill", "Cisneros", "Crow", "Shapiro",
               "Baer", "Fletcher", "Jones", "Allred", "Delgado", 
               "Brindisi", "Kelly", "Davis", "Davids", "Feehan",
               "Radinovich", "Pureval", "O'connor")
lastnames <- lastnames[order(lastnames)]

spend_df$party <- ifelse(spend_df$party == "DFL", "DEM", spend_df$party)
spend_df$party_full <- ifelse(spend_df$party_full == "DEMOCRATIC-FARM-LABOR", "DEMOCRATIC PARTY",  spend_df$party_full)

spend_df <- spend_df %>% filter(Last_Name %in% lastnames) %>% 
                         filter(name != "Kevin Jones") %>%
                         select(party, party_full, state, district, receipts, disbursements, cash_on_hand_end_period, 
                         debts_owed_by_committee, federal_funds_flag, has_raised_funds, Last_Name, First_Name, 
                         District_ID) 
          

spend_df_dem <- spend_df %>% filter(party == "DEM") %>% arrange(state, district)
spend_df_rep <- spend_df %>% filter(party == "REP") %>% arrange(state, district)

merger <- NULL
merger$District_ID <- paste0(spend_df_dem$state, spend_df_dem$district)
merger$Total_Spending <- spend_df_dem$disbursements + spend_df_rep$disbursements
merger$Total_Spending_Dem <- spend_df_dem$disbursements
merger$Total_Spending_Rep <- spend_df_rep$disbursements
merger$Total_Receipts <- spend_df_dem$receipts + spend_df_rep$receipts
merger$Depts_Comittee_Dem <- spend_df_dem$debts_owed_by_committee
merger$Depts_Comittee_Rem <- spend_df_rep$debts_owed_by_committee
merger$Federal_Funds_Dem <- spend_df_dem$federal_funds_flag
merger$Federal_Funds_Rep <- spend_df_rep$federal_funds_flag

merger <- as.data.frame(merger)
################################################################################################
# 5) Merge Data
################################################################################################
df <- merge(ele_df, merger, by = "District_ID", all.x = T)

setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Data on US States")

write_csv(df, "Election_Results_Table_fin.csv")

################################################################################################