##############################################
##
## Airline Classification Example for Client Deployment
## 
## Author: Janae.Nicholson@hrblock.com
## Date: October 19, 2017
##
#############################################

library(sparklyr)
library(dplyr)
library(tidyr)
#library(broom)
library(lubridate)
library(ggplot2)
sc <- spark_connect(master = "local")

#data from: https://s3.amazonaws.com/h2o-airlines-unpacked/airlines_all.05p.csv
#this is alot of data for my local machine
#Keep only the columns I need
airlines_local <- read.csv(file = "C:/Users/janae/Documents/RProj/datasets/airlines_all.05p.csv",
                           stringsAsFactors = FALSE) %>% 
  select(IsDepDelayed, Origin, Dest, UniqueCarrier,
         Year, DayofMonth, DayOfWeek, Month, Distance)

weather_local <- read.csv(file = "C:/Users/janae/Documents/RProj/datasets/Chicago_Ohare_International_Airport.csv", 
                          stringsAsFactors = FALSE)
weather_local <- weather_local %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         DayofMonth = day(Date)) %>% 
  select(-Date)
#head(weather_local) 

airlines_sdf <- copy_to(sc, airlines_local, "airlines_sdf",
                        overwrite = TRUE)
#sdf_dim(airlines_sdf)

weather_sdf <- copy_to(sc, weather_local, "weather_sdf", overwrite = TRUE)
head(weather_sdf)

ohare_sdf <- airlines_sdf %>% 
  filter(Origin=="ORD" & Year >= 2005) %>% 
  left_join(weather_sdf, by = c("Year" = "Year", 
                                "Month" = "Month",
                                "DayofMonth" = "DayofMonth")) %>% 
  mutate(IsDepDelayed = ifelse(IsDepDelayed == "YES", 1, 0), 
         DayOfWeek = as.character(DayOfWeek),
         Month = as.character(Month),
         PrcpIn = ifelse(is.na(PrcpIn) | PrcpIn == "T", 0, as.numeric(PrcpIn)),
         SnowIn = ifelse(is.na(SnowIn) | SnowIn == "T", 0, as.numeric(SnowIn))) 
sdf_dim(na.omit(ohare_sdf))
ohare_head <- collect(head(ohare_sdf, 10))
sdf_register(ohare_sdf, "ohare_sdf")
# head(ohare_sdf)

#since we are running multiple models prepare the data
prepared <- ml_prepare_features(ohare_sdf,
                             features = c("Dest", "UniqueCarrier",
                                          "DayOfWeek", "Month"))

#split the data into training and test.
prepared <- prepared %>% 
  sdf_partition(training = 0.7, test = 0.2, validation = 0.1, 
                seed = 101917)

train_sdf <- prepared$train
test_sdf <- prepared$test
validation_sdf <- prepared$validation

airY <- "IsDepDelayed"
airX <- c("Dest", "Year", "DayOfWeek", "Month", "UniqueCarrier", "Distance", 
          "TmaxF", "TminF", "TmeanF", "PrcpIn", "SnowIn",
          "CDD", "HDD", "GDD")

reg_fit <- ml_logistic_regression(train_sdf,
                                  response = airY,
                                  features = airX)
#summary(reg_fit)
# #not yet supported
# glance(reg_fit)
# reg_coef <- tidy(reg_fit)
# test_aug <- augment(reg_fit)
#summary(reg_fit) #warning lots of coefficents

tree_fit <- ml_decision_tree(train_sdf,
                             response = airY,
                             features = airX,
                             impurity = "gini",
                             type = "classification")

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

score_sdf <- sdf_predict(rf_fit, validation_sdf) %>% 
  sdf_separate_column("probability", list("P_Delayed" = 2)) %>% 
  select(IsDepDelayed, Dest, Year, DayOfWeek, Month, 
         UniqueCarrier, Distance, TmaxF, TminF, TmeanF, 
         PrcpIn, SnowIn, CDD, HDD, GDD, P_Delayed)
sdf_dim(score_sdf)

spark_disconnect(sc)
