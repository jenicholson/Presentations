##############################################
##
## Airline Classification Example for Client Deployment
## 
## Author: Janae.Nicholson@hrblock.com
## Date: October 19, 2017
##
#############################################

#load necessary packages
library(sparklyr)
library(dplyr)
library(tidyr)
#library(broom) #not supported yet
library(lubridate)
library(ggplot2)

#make sure to use Spark 2.0 or greater for full functionality
#Set the version of Spark to use since multiple installed
Sys.setenv("SPARK_HOME"="/opt/cloudera/parcels/SPARK2-2.2.0.cloudera1-1.cdh5.12.0.p0.142354/lib/spark2/")
Sys.setenv("SPARK_HOME_VERSION"="2.2.0")

#Connect to Spark
sc <-
  spark_connect(master = "yarn-client", config = list(
    spark.submit.deployMode = "client"
  ))

# Airline data from http://stat-computing.org/dataexpo/2009/the-data.html
# Data data is stored as a CSV by year in hdfs w/o hive projection
# Read the files into Spark and create one spark data frame
flight_files <- c('/user/janae/airlinecsv/2008.csv', 
                  '/user/janae/airlinecsv/2007.csv', 
                  '/user/janae/airlinecsv/2006.csv', 
                  '/user/janae/airlinecsv/2005.csv')
flight_names <- c('flights08', 'flights07', 'flights06', 'flights05')

#now loop through all the files and rowbind the results
#create a list to hold the data frames from each file
hold_df <- vector("list", length(flight_files))
for(ii in 1:length(flight_files)) {
  #read in data and store in the  list of dataframes
  hold_df[[ii]] <- spark_read_csv(sc, name = flight_names[ii],
                                  path = flight_files[ii])
}
#now make one data frame out of the list of data frames
flights_sdf <- do.call("sdf_bind_rows", hold_df)
sdf_register(flights_sdf, "flights_sdf")

#read in a local weather file
#Work with dates in lubridate (much easier than Spark)
weather_local <- read.csv(file = "data/airline_data/Chicago_Ohare_International_Airport.csv", 
                          stringsAsFactors = FALSE)
weather_local <- weather_local %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         DayofMonth = day(Date)) %>% 
  select(-Date)
#head(weather_local) 

#copy local data to Spark via dplyr command
weather_sdf <- copy_to(sc, weather_local, "weather_sdf", 
                       overwrite = TRUE)
head(weather_sdf)

#Manipulate the Data
ohare_sdf <- flights_sdf %>% 
  filter(Origin=="ORD" & Year >= 2005 & DepTime != "NA" & ArrTime != "NA") %>% 
  left_join(weather_sdf, by = c("Year" = "Year", 
                                "Month" = "Month",
                                "DayofMonth" = "DayofMonth")) %>% 
  mutate(DepTime = as.numeric(DepTime), 
         ArrTime = as.numeric(ArrTime),
         DepDelay = ifelse(DepDelay != "NA", as.numeric(DepDelay), 0),
         IsDepDelayed = ifelse(DepDelay > 0, 1, 0),
         DayOfWeek = as.character(DayOfWeek),
         Month = as.character(Month),
         PrcpIn = ifelse(is.na(PrcpIn) | PrcpIn == "T", 0, as.numeric(PrcpIn)),
         SnowIn = ifelse(is.na(SnowIn) | SnowIn == "T", 0, as.numeric(SnowIn))) %>% 
  select(IsDepDelayed, Dest, Year, DayOfWeek, Month, UniqueCarrier, Distance, 
         TmaxF, TminF, TmeanF, PrcpIn, SnowIn,
         CDD, HDD, GDD)
sdf_dim(na.omit(ohare_sdf))
ohare_head <- collect(head(ohare_sdf, 10))
sdf_register(ohare_sdf, "ohare_sdf")
# head(ohare_sdf)

#Create Dummy Variables
prepared <- ml_prepare_features(ohare_sdf, 
                                features = c("Dest", "UniqueCarrier",
                                             "DayOfWeek", "Month"))

#split the data into training, test, and validation
prepared <- prepared %>% 
  sdf_partition(training = 0.7, test = 0.2, validation = 0.1, seed = 101917)

train_sdf <- prepared$train
test_sdf <- prepared$test
validation_sdf <- prepared$validation
# sdf_dim(train_sdf)
# sdf_dim(test_sdf)
# sdf_dim(validation_sdf)

#set up dependent and independent variables
#Can also use R formulas
airY <- "IsDepDelayed"
airX <- c("Dest", "Year", "DayOfWeek", "Month", "UniqueCarrier", "Distance", 
          "TmaxF", "TminF", "TmeanF", "PrcpIn", "SnowIn",
          "CDD", "HDD", "GDD")

#fit a logistic regression
reg_fit <- ml_logistic_regression(train_sdf,
                                  response = airY,
                                  features = airX)

#fit a decision tree
tree_fit <- ml_decision_tree(train_sdf,
                             response = airY,
                             features = airX,
                             impurity = "gini",
                             type = "classification")

#fit a small random forest
rf_fit <- ml_random_forest(x = train_sdf, 
                           response = airY,
                           features = airX,
                           impurity = "gini",
                           num.trees = 100,
                           type = "classification",
                           seed = 10432)

#create a list of models
ml_models <- list(
  "Logistic" = reg_fit,
  "Decision Tree" = tree_fit,
  "Random Forest" = rf_fit
)

# Create a function for scoring
score_test_data <- function(model, data=test_sdf){
  sdf_predict(model, data) %>%
    select(IsDepDelayed, prediction, probability)
}

# Score all the models
ml_score <- lapply(ml_models, score_test_data)

# Function for calculating accuracy
calc_accuracy <- function(data, cutpoint = 0.5){
  data %>%
    sdf_separate_column("probability", list("P_YES" = 2)) %>% 
    mutate(pred = if_else(P_YES > cutpoint, 1.0, 0.0),
           prediction = as.double(pred),
           tgt = ifelse(IsDepDelayed == 1,1,0),
           IsDepDelayed = as.double(tgt)) %>%
    ml_classification_eval(label = "IsDepDelayed", predicted_lbl = "prediction", metric = "accuracy")
}

# Calculate AUC
perf_metrics <- data.frame(
  model = names(ml_score),
  AUC = 100 * sapply(ml_score, ml_binary_classification_eval,
                     label = "IsDepDelayed", score = "probability"),
  Accuracy = 100 * sapply(ml_score, calc_accuracy),
  row.names = NULL, stringsAsFactors = FALSE)

perf <- gather(perf_metrics, metric, value, AUC, Accuracy) %>%
  mutate(val2=round(value,1))

# plot a comparison of AUC
gg2 <- ggplot(perf, aes(reorder(model, value), value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=val2),vjust=-0, hjust=1.1, colour="white",
            position=position_dodge(.9), size=4) +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=c("#C9C9C9", "#89BDD3")) +
  xlab("") +
  ylab("Percent") +
  ggtitle("Performance Metrics")
gg2

#run feature importance
# Initialize results
feature_importance <- data.frame()

# Calculate feature importance
for(i in c("Decision Tree", "Random Forest")){
  feature_importance <- ml_tree_feature_importance(sc, ml_models[[i]]) %>%
    mutate(Model = i) %>%
    mutate(importance = as.numeric(levels(importance))[importance]) %>%
    mutate(feature = as.character(feature)) %>%
    rbind(feature_importance, .)
}

##need to clean up the results
top_features <- feature_importance %>% 
  group_by(Model) %>% 
  arrange(Model, desc(importance)) %>%
  slice(1:10) %>% 
  ungroup() 
keep_features <- unique(top_features$feature)
clean_features <- feature_importance %>% 
  filter(feature %in% keep_features)

# Plot results
clean_features %>%
  ggplot(aes(reorder(feature, importance), importance, fill = Model)) + 
  facet_wrap(~Model) +
  geom_bar(stat = "identity") + 
  theme_bw() +
  scale_fill_manual(values=c("#C9C9C9", "#89BDD3")) +
  coord_flip() +
  xlab("") +
  ggtitle("Top Feature Importance")

#score validation set with random forest
#only keep original data and prediction, can't save dummy vars to csv
score_sdf <- sdf_predict(rf_fit, validation_sdf) %>% 
  sdf_separate_column("probability", list("P_Delayed" = 2)) %>% 
  select(IsDepDelayed, Dest, Year, DayOfWeek, Month, 
         UniqueCarrier, Distance, TmaxF, TminF, TmeanF, 
         PrcpIn, SnowIn, CDD, HDD, GDD, P_Delayed)
sdf_dim(score_sdf)

# reduce the number of partitions to 1 and store as a csv in hdfs
score_sdf %>% sdf_coalesce(1) %>%
  spark_write_csv("/user/janae/results/Scored_Results",
                  mode = "overwrite")

#pull the data to the edge node
#Note: You will not have control over the name of the csv file
#Best to put in dedicated directory
system("hdfs dfs -get /user/janae/results/Scored_Results /home/janae/RProj/Sparklyr_Demo/data/results/")

spark_disconnect(sc)