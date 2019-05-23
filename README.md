# Master Thesis Code:
This Repository contains all the scripts used to do the data collection and analysis for my master thesis called: "How and when are Social-Bots used in social media during election campaigns?" Unfortunately this only includes the scripts but no data since even the processed data is still several Gigabytes, which makes it quite difficult to make it accessible. Hence if you are interested in the data you need to cantact me directly at mael.kubli:at:uzh.ch

## List of Scripts Used: 
  - AWS_Colector.R (Script used to stream the tweets for Twitter on an Amazon AWS server with RStudio)
  - District_Election_Data.R (Data form the districts sampled)
  - Data_Processing (Script containing all the functions and Processes to combine the data and prepare it for classification)
  - Deep_Neural_Network_Training.R (Building the Classifier with H2O)
  - Classification_of_Data.R (Classify tweets)
  - Data_Analysis.R
  - Data_Analysis_II.R
  - Data_analysis_II.R
  - Botwiki_MYClassifier.R
  
  
## Data 
I only provide all the  data from the best classification model and the data used to test this Classifier on the bots I found on botwiki.org
Therefore you can at least replicate the script (Botwiki_MYClassifier.R) which I used to test if the classifier works with known bots on twitter. 
  
  
