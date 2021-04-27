library(dslabs)
library(tidyverse)
library(DescTools)
library(caret)
if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
options(digits = 5)

# Setting the seed for reproducible results
set.seed(2021, sample.kind = "Rounding")

# Partitioning the edx dataset into training (80%) and testing sets (20%)
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Ensuring the test set contains movies and users contained in the training set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Creating the overall average for all users and all movies
all_avgs <- mean(train_set$rating)

# Creating a data frame to store RMSEs beginning with the naive RMSE
rmse_results <- data.frame(model = "Naive", rmse = RMSE(test_set$rating, all_avgs))

# Creating the movie averages for use as the movie bias
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - all_avgs))

# Creating the user averages for use as the user bias
b_u <- train_set %>% 
  left_join(b_i, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - all_avgs - b_i))

# Predicting ratings with user and movie biases included and adding RMSE to the data frame
biases_predictions <- test_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred = all_avgs + b_i + b_u) %>%
  pull(pred)

rmse_results <- rbind(rmse_results, c("With Biases", RMSE(test_set$rating, biases_predictions)))

# Converting the train and test datasets into recosystem-compatible matrices
train_reco <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating = rating))
test_reco  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating = rating))

# Resetting the seed for reproducible results
set.seed(2021, sample.kind = "Rounding")

# Creating the recosystem object
fit_reco <-  recosystem::Reco()

# Fitting the recosystem object to the training data
fit_reco$train(train_reco)

# Using our fitted recosystem object to make predictions against the test set; designating
# our computer's memory as the output destination as it defaults to text file
reco_predictions <-  fit_reco$predict(test_reco, out_memory())

# Calculating the RMSE of our test data and adding it to the data frame
rmse_results <- rbind(rmse_results, c("Matrix Factorization", RMSE(test_set$rating, reco_predictions)))

# Displaying the RMSE results formatted with knitr
rmse_results %>% knitr::kable()

# Resetting the seed for reproducible results
set.seed(2021, sample.kind = "Rounding")

# Converting the unpartitioned edx and validation datasets into recosystem-compatible
# matrices
edx_reco <-  with(edx, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating = rating))
validation_reco  <-  with(validation,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating = rating))

# Fitting the recosystem object to the entire edx dataset
fit_reco$train(edx_reco)

# Using the recosystem object to make predictions against the validation dataset
y_hat <- fit_reco$predict(validation_reco, out_memory())

# Calculating and displaying the RMSE of our recosystem model against the validation dataset
validation_rmse <- RMSE(validation$rating, y_hat)
validation_rmse

# Exporting our results for use in the markdown document
save(rmse_results, validation_rmse, file = "data.RData")