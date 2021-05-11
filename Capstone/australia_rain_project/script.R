if (!require(dplyr)) install.packages('dplyr')
if (!require(caret)) install.packages('caret')
if (!require(rpart)) install.packages('rpart')
if (!require(rpart.plot)) install.packages('rpart.plot')
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

##### PLEASE NOTE #####
# I've commented out all training of machine learning models below with the exception
# of the final optimized decision tree model.  As-is, this code should take under
# 60 seconds to run on a decently spec'd PC.  Should you decide to un-comment the
# remaining four models, be warned that it may take upwards of 30 minutes to run.

# Saving the current system time  for use in calculating the time it takes to run the code
start_time <- Sys.time()

# Downloading the data from my GitHub repository and reading it to memory
# Note the original source may be found here: https://www.kaggle.com/jsphyg/weather-dataset-rattle-package
url <- "https://raw.githubusercontent.com/kittela/HarvardX/main/Capstone/australia_rain_project/weatherAUS.csv"
download.file(url, destfile = basename(url))
rain_data <- read.csv(basename(url))

# Viewing columns and a sample of observations to get a basic understanding of the dataset
head(rain_data)

# Viewing how many observations of each variable contains NAs
na_count <- sapply(rain_data, function(x) sum(length(which(is.na(x)))))
data.frame(names = names(na_count), na_count) %>% 
  ggplot(aes(x = names, y = na_count, fill = na_count)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  xlab("Variable") +
  ylab("NA Count") +
  scale_fill_gradient(low = "blue", high = "red")

# Cleaning the data by assuming a missing report of rain means no rain, factorizing 
# all discrete variables, then removing the remaining observations with NA values
rain_clean <- rain_data %>% 
  select(-Cloud3pm, -Cloud9am, -Evaporation, -Sunshine) %>%
  mutate(RainToday = factor(ifelse(is.na(RainToday),"No",RainToday)),
         RainTomorrow = factor(ifelse(is.na(RainTomorrow),"No",RainTomorrow)),
         Location = factor(Location),
         WindGustDir = factor(WindGustDir)) %>%
  na.omit()

# Setting the seed for reproducible results
set.seed(2021, sample.kind = "Rounding")

# Partitioning the dataset into training (80%) and testing sets (20%)
test_index <- createDataPartition(y = rain_clean$RainTomorrow, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- rain_clean[-test_index,]
test_set <- rain_clean[test_index,]

# Determining the percentage of days with rain in our training dataset
chance_of_rain <- mean(train_set$RainToday == "Yes")
chance_of_rain

# Guessing it will rain tomorrow with 50% odds
guess_y_hat <- factor(sample(c("Yes","No"), nrow(test_set), replace = TRUE)) %>%
  factor()
confusionMatrix(guess_y_hat, test_set$RainTomorrow)$overall['Accuracy']

# Guessing it will rain tomorrow based on the overall chance of rain
guess_y_hat <- factor(sample(c("Yes","No"), nrow(test_set), replace = TRUE, prob = c(chance_of_rain, 1 - chance_of_rain))) %>%
  factor()
confusionMatrix(naive_y_hat, test_set$RainTomorrow)$overall['Accuracy']

# Guessing it will rain tomorrow with 0% odds and viewing full confusion matrix
# results.  Note the 100% sensitivity and 0% specificity.
guess_y_hat <- factor(sample(c("Yes","No"), nrow(test_set), replace = TRUE, prob = c(0,1))) %>%
  factor(levels = c("No", "Yes"))
confusionMatrix(guess_y_hat, test_set$RainTomorrow)

# Checking the F-Score
F_meas(guess_y_hat, reference = test_set$RainTomorrow)

# Fitting linear model
# linear_fit <- train(RainTomorrow ~ ., data = train_set, method = "glm")
# linear_y_hat <- predict(linear_fit, test_set) %>% factor(levels = c("No", "Yes"))
# confusionMatrix(linear_y_hat, test_set$RainTomorrow)$overall['Accuracy']
# F_meas(linear_y_hat, reference = test_set$RainTomorrow)

# Fitting linear discriminant analysis model
# lda_fit <- train(RainTomorrow ~ RainToday + Humidity3pm + Rainfall, data = train_set, method = "lda")
# lda_y_hat <- predict(lda_fit, test_set) %>% factor(levels = c("No", "Yes"))
# confusionMatrix(lda_y_hat, test_set$RainTomorrow)$overall['Accuracy']
# F_meas(lda_y_hat, reference = test_set$RainTomorrow)

# Fitting decision tree model
# dt_fit <- train(RainTomorrow ~ RainToday + Humidity3pm + Rainfall, data = train_set, method = "rpart")
# dt_y_hat <- predict(dt_fit, test_set) %>% factor(levels = c("No", "Yes"))
# confusionMatrix(dt_y_hat, test_set$RainTomorrow)$overall['Accuracy']
# F_meas(dt_y_hat, reference = test_set$RainTomorrow)

# Fitting random forest model
# rf_fit <- train(RainTomorrow ~ RainToday + Humidity3pm + Rainfall, data = train_set, method = "rf")
# rf_y_hat <- predict(rf_fit, test_set) %>% factor(levels = c("No", "Yes"))
# confusionMatrix(rf_y_hat, test_set$RainTomorrow)$overall['Accuracy']
# F_meas(rf_y_hat, reference = test_set$RainTomorrow)

# Optimizing the decision tree model
dt_fit <- train(RainTomorrow ~ RainToday + Humidity3pm + Rainfall, data = train_set, method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.005)))
dt_fit$results$cp[which.max(dt_fit$results$Accuracy)]
ggplot(dt_fit, highlight = TRUE)
dt_y_hat <- predict(dt_fit, test_set) %>% factor(levels = c("No", "Yes"))
confusionMatrix(dt_y_hat, test_set$RainTomorrow)$overall["Accuracy"]
F_meas(dt_y_hat, reference = test_set$RainTomorrow)

# Plotting the decision tree
rpart.plot(dt_fit$finalModel, box.palette="RdBu", shadow.col="gray", nn=TRUE)

# Saving data for use in markdown document
save(rain_data, dt_fit, file = "data.RData")

# Printing the time it took this code to run
paste("Time to run:", round(Sys.time() - start_time, 0), "seconds")