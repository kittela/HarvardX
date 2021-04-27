library(dplyr)
library(ggplot2)

# How many rows and columns are there in the edx dataset?

# Number of rows:
nrow(edx)

# Number of columns:
ncol(edx)

# How many zeros were given as ratings in the edx dataset?
sum(edx$rating == 0)

# How many threes were given as ratings in the edx dataset?
sum(edx$rating == 3)

# How many different movies are in the edx dataset?
length(unique(edx$movieId))

# How many different users are in the edx dataset?
length(unique(edx$userId))

# How many movie ratings are in each of the following genres in the edx dataset?

# Drama:
sum(grepl("Drama", edx$genres))

# Comedy:
sum(grepl("Comedy", edx$genres))

# Thriller:
sum(grepl("Thriller", edx$genres))

# Romance:
sum(grepl("Romance", edx$genres))

# Which movie has the greatest number of ratings?
edx %>% group_by(movieId, title) %>%
  summarize(count = dplyr::n()) %>%
  arrange(desc(count))

# What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% 
  summarize(count = dplyr::n()) %>% 
  arrange(desc(count))

# True or False: In general, half star ratings are less common than whole star ratings 
# (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
  