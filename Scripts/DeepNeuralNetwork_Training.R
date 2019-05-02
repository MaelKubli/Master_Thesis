###################################################################################################
# Deep Learning Network Training 
###################################################################################################
# Description
###################################################################################################
# This script contain the whole setup and training for the deep neural network (DNN) to classify 
# a user is some sort of a bot or a genuine user. The DNN is trained not with R but with the freely
# avilable machine learning software H2O. This software is useable throug a package in R. The great
# advantage of this software is the expanded flexibility and scalability it posseses over R native 
# libraries like cared or nnet.
###################################################################################################
# Content
###################################################################################################
# 1) Dependencies
# 2) Start H2O
# 3) Load Training Data and transform variables to right class
# 4) Split data in Training / Test / Valid -Set
# 5) Prepare Data for Training 
# 6) Random Grid Search
## 6.0) Layers and Activation function Grid Search
## 6.1) Setup of Random Grid Search For best model
## 6.2) Train and validate a grid of DNNs
# 7) Retreive best models for decision which to use for prediction
## 7.1) Look at model output in H2O's GUI 
## 7.2) Extract all there is to extract from Models and save Models for Prediction
# 8) Terminate H2O
###################################################################################################
# 1) Dependencies
###################################################################################################
library(h2o)
library(caret)
library(randomForest)
library(Hmisc)
library(data.table)
library(readr)
library(dplyr)
library(ggplot2)
library(stargazer)
###################################################################################################
# 2) Start H2O
###################################################################################################
# Start an H2O using one node with all cores minus one core and as much GB of RAM as you have. 
#I use 16 cores with 32 GB but it will also work with less cores and only 4 GB of RAM. 
# The downside then is that the training takes much longer for the same amount of models 

# Leave at least one core for your system mabe even two if you want to do other stuff on it. 
h2oServer <- h2o.init(ip="localhost", port=54321, max_mem_size="48g", nthreads=12)
###################################################################################################
# 3) Load Training Data and transform variables to right class
###################################################################################################
#Change dir to path where the training data set is: /Users/Mael/
workdir="D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data"
#workdir="/Users/Mael/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data"


setwd(workdir)

# Load Training Data on the User Level, since it would not make much sense to predict each tweet by it self, 
# because a user is either a bot or he is not. This can't change between tweets or at least it shouldn't.
data <- h2o.importFile(path = "D:/CloudStation/Universitaet Zuerich/Master Thesis/Data/Training Data/TrainingDataPro_User.csv", destination_frame = "data")

# Get dimension of data set:
dim(data)

# Since I want to do a binary classification I have to make sure that the dependent variable is a factor of 
# 0 or 1 otherwise H2O would see it as a numeric value between 0 and 1 
data$Bot <- as.factor(data$Bot)

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
# 4) Split data in Training / Test / Valid -Set
###################################################################################################
# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = data, 
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 1848)            #setting a seed will guarantee reproducibility
train <- splits[[1]]
valid <- splits[[2]]
test <-  splits[[3]]

# Take a look at the size of each partition
# Notice that h2o.splitFrame uses approximate splitting not exact splitting (for efficiency)
# so these are not exactly 70%, 15% and 15% of the total rows
nrow(train)  
nrow(valid) 
nrow(test)  
###################################################################################################
# 5) Prepare Data for Training 
###################################################################################################
# "Remove" Variables which are not nnumerical and thus not used for the prediction:
# Identify response and predictor variables
y <- "Bot"
x <- setdiff(names(data), c(y, "user_id", "status_id",  "screen_name", "lang", 
                            "name", "location", "account_lang"))  # remove these variables because they are only 
                                                                  # needed for the identification of the user or not usefull for a DNN
print(x)
###################################################################################################
# 6) Random Grid Search
###################################################################################################
# Now that the data is prepared, we can train some models Rather than training models manually 
# one-by-one or searching through a large Grid, which takes far too long, I will make use of the 
# random grid searh method.
###################################################################################################
## 6.0) Layers and Activation function Grid Search
###################################################################################################
# Activation Functions to consider for the DNN taining:
activation_opt <- c("RectifierWithDropout", "MaxoutWithDropout")

hidden_para <- list(c(25,10), c(25,5), c(20,10,5), c(15,10,5), c(25,20,15,10), c(20,20,10,10))

#Hyper Parameter Grid for Random Search for a good DNN:
hyper_params <- list(activation = activation_opt,
                     hidden = hidden_para)


# Search Criteria (Random Discrete Search)
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_runtime_secs = 3600,
                        stopping_tolerance=1e-2)

# Write Output in this directory
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/GridSearch_Level_1")

# Train Models at random within the specified hyperparameter grid:
dl_grid <- h2o.grid("deeplearning", x = x, y = y,
                    grid_id = "dl_grid",
                    training_frame = train,
                    validation_frame = valid,
                    nfolds = 5,
                    fold_assignment = "Stratified",
                    standardize = TRUE, # Needed since not all Varibles are of same scale
                    seed = 1991,
                    distribution = "AUTO",
                    stopping_metric = "logloss",
                    shuffle_training_data = TRUE,
                    ignore_const_cols = TRUE,
                    missing_values_handling = "MeanImputation",
                    diagnostics = TRUE, # Variable importances for input features 
                    adaptive_rate = TRUE,
                    hyper_params = hyper_params,
                    search_criteria = search_criteria,
                    stopping_rounds=3)

# Get performance of each tested model configuration back orderd by their logloss value starting with 
# the best model
dl_gridperf <- h2o.getGrid(grid_id = "dl_grid", 
                           sort_by = "logloss", 
                           decreasing = F)

### Save Grid Summary
print(dl_gridperf)
gridtable <- dl_gridperf@summary_table
write.csv(gridtable, "Random_Grid_Layer_Performance.csv")

### Average Logloss ReLU vs. Maxout
gridtable$logloss <- as.numeric(gridtable$logloss)
activationtable <- gridtable %>% group_by(activation) %>% summarise( Mean_Ll = mean(logloss), SD_Ll = sd(logloss), Var_Ll = var(logloss))
write.csv(activationtable, "Random_Grid_Activation_Performance.csv")

###################################################################################################
## 6.1) Setup of Random Grid Search For best model
###################################################################################################
# Lasso- and and Ridge-Regression Regularization: 
l1_opt <- c(1e-8,1e-7,1e-6)
l2_opt <- c(1e-8,1e-7,1e-6)

# Hidden (Specify the hidden layer sizes) possible for training:
# More than three hidden layers are risky due to overfitting...
# More than 2/3 of features as number of neurons in layer is bad as well for overfitting
hidden_para <- list(c(30,5),c(25,5),c(30,3),c(25,3))

# Specify the hidden layer dropout ratio to improve generalization. 
# Specify one value per hidden layer. The range is >= 0 to <1
hidden_d_r <- list(c(.3,.3),c(.4,.4))

# Specify the input layer dropout ratio to improve generalization. 
# Suggested values are 0.1 or 0.2.
input_1_d_r <- c(0.1,.15,0.2)

# Stopping Tolerance: Specify the relative tolerance for the 
# metric-based stopping to stop training if the improvement 
# is less than this value.
stopping_tol <- c(0.02,0.03,0.04)

# Epochs
epo <- c(2000,3000.4000)


#Hyper Parameter Grid for Random Search for a good DNN:
hyper_params <- list(l1 = l1_opt,
                     l2 = l2_opt,
                     hidden = hidden_para,
                     input_dropout_ratio = input_1_d_r,
                     hidden_dropout_ratios  = hidden_d_r,
                     stopping_tolerance = stopping_tol,
                     epochs = epo)

# Search Criteria (Random Discrete Search)
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_runtime_secs = 10800)
###################################################################################################
## 6.2) Train and validate a grid of DNNs
###################################################################################################
# Train Models at random within the specified hyperparameter grid:
dl_grid <- h2o.grid("deeplearning", x = x, y = y,
                    grid_id = "dl_grid",
                    training_frame = train,
                    validation_frame = valid,
                    nfolds = 5,
                    fold_assignment = "Stratified",
                    standardize = TRUE, # Needed since not all Varibles are of same scale
                    seed = 1991,
                    distribution = "AUTO",
                    activation = "RectifierWithDropout",
                    stopping_metric = "logloss",
                    shuffle_training_data = TRUE,
                    ignore_const_cols = TRUE,
                    missing_values_handling = "MeanImputation",
                    diagnostics = TRUE, # Variable importances for input features
                    max_w2=2,          # Can help improve stability for Rectifier
                    adaptive_rate = TRUE,
                    hyper_params = hyper_params,
                    search_criteria = search_criteria,
                    stopping_rounds=3)

# Get performance of each tested model configuration back orderd by their logloss value starting with 
# the best model
dl_gridperf <- h2o.getGrid(grid_id = "dl_grid", 
                           sort_by = "logloss", 
                           decreasing = F)

summary(dl_gridperf, show_stack_traces = FALSE)
###################################################################################################
# 7) Retreive best models for decision which to use for prediction
###################################################################################################
## 7.1) Look at model output in H2O's GUI 
###################################################################################################
# Open GUI
# http://localhost:54321
###################################################################################################
## 7.2) Extract all there is to extract from Models and save Models for Prediction
###################################################################################################
## Change Dir to Output:
setwd("D:/CloudStation/Universitaet Zuerich/Master Thesis/Output/GridSearch_Step_2_v2")

### Save Grid Summary
print(dl_gridperf)
gridtable <- dl_gridperf@summary_table
write.csv(gridtable, "Random_Grid_All_Models.csv")

### Get the average logloss of the different types of network sizes:
gridtable$logloss <- as.numeric(gridtable$logloss) 
avgnetsize <- gridtable %>% group_by(hidden, input_dropout_ratio) %>% summarise( Mean_Ll = mean(logloss), SD_Ll = sd(logloss))


## Make some outputs of the best 15 models:
for(i in 1:25){
  best_dl_model_id <- dl_gridperf@model_ids[[i]] 
  best_dl <- h2o.getModel(best_dl_model_id)
  
  perf_traindata <-   h2o.performance(best_dl, newdata = data)
  perf_testdata <- h2o.performance(model = best_dl, newdata = test)
  perf_validdata <- h2o.performance(best_dl, newdata=valid)
  
  #Get Model Name:
  modname <- perf_validdata@metrics[["model"]][["name"]]
  
  #Confusion Matrix of ith model:
  conmat <- h2o.confusionMatrix(perf_validdata)
  write.csv(conmat, paste0("Validation_Data_Confusion_Matrix_",modname,".csv"))
  stargazer::stargazer(conmat, summary = F, header = F, type = "html", out = paste0("Validation_Data_Confusion_Matrix_", i, "_", modname,".html"))
  
  conmat <- h2o.confusionMatrix(perf_testdata)
  write.csv(conmat, paste0("Test_Data_Confusion_Matrix_",modname,".csv"))
  stargazer::stargazer(conmat, summary = F, header = F, type = "html", out = paste0("Test_Data_Confusion_Matrix_", i, "_", modname,".html"))
  
  conmat <- h2o.confusionMatrix(perf_traindata)
  write.csv(conmat, paste0("Full_Data_Confusion_Matrix_",modname,".csv"))
  stargazer::stargazer(conmat, summary = F, header = F, type = "html", out = paste0("Full_Data_Confusion_Matrix_", i, "_", modname,".html"))
  
  #Variable importance of best model:
  h2o.varimp_plot(best_dl, num_of_features = 35)
  varimplist <- h2o.varimp(best_dl)
  write.csv(varimplist, paste0("Variable_Importance_",modname,".csv"))
  stargazer::stargazer(varimplist, summary = F, header = F, type = "html", out = paste0("Variable_Importance_", i, "_", modname,".html"))
  
  # Save Model Parameters:
  params <- best_dl@parameters
  write_rds(params, paste0("Parameter_", i, "_", modname,".rds"))
  
  # Save Model Validation Metrics:
  params <- perf_validdata@metrics
  write_rds(params, paste0("Metrics_Validation_Set_", i, "_", modname,".rds"))
  
  # Save Model Test Metrics:
  params <- perf_testdata@metrics
  write_rds(params, paste0("Metrics_Test_Set_", i, "_", modname,".rds"))
  
  # Save Model full data Metrics:
  params <- perf_testdata@metrics
  write_rds(params, paste0("Metrics_full_data_Set_", i, "_", modname,".rds"))
  
  # save the model
  model_path <- h2o.saveModel(object=best_dl, path=getwd(), force=TRUE)
  print(model_path)
  
}
###################################################################################################
# 8) Terminate H2O
###################################################################################################
h2o.shutdown(prompt = FALSE)
###################################################################################################
