##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

# install packages if needed

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# load libraries

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# download movielens data set and save to temprorary file

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# read ratings.dat file and store in ratings

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# read movies.dat file and store in movies

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

# set name of columns/fields in movies

colnames(movies) <- c("movieId", "title", "genres")


# if using R 3.6 or earlier:
# type cast column 
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))

# if using R 4.0 or later:
# type cast column 
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))



# join ratings and movies using movieId as key

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

# slice movielens to create training set (edx) and temp set

edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# remove unnecessary variable and data sets
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# establish release year. process time stamp. create fields for year and month ratings were made
edx <-
  edx %>%
  mutate(rating_time =
           as.Date(as.POSIXct(timestamp, origin = "1970-01-01")),
         rating_year = year(rating_time),
         rating_month = month(rating_time),
         release_year = as.integer(
           substr(title, str_length(title) - 4, str_length(title) - 1)
         )) %>%
  select(-timestamp, -rating_time)

validation <-
  validation %>%
  mutate(rating_time =
           as.Date(as.POSIXct(timestamp, origin = "1970-01-01")),
         rating_year = year(rating_time),
         rating_month = month(rating_time),
         release_year = as.integer(
           substr(title, str_length(title) - 4, str_length(title) - 1)
         )) %>%
  select(-timestamp, -rating_time)

######################################################################
# Explore and analyse training set - user, movie, release year, rating
######################################################################

#show first 6 rows of the training set (edx) to get insight into the table and data structure
head(edx)
# show summary statistics of edx data
summary(edx)
# edx data set has 9,000,055 records and 6 fields

#show first 6 rows of the validation set to get insight into the table and data structure
head(validation)
# show summary statistics of validation data
summary(validation)
# Validation data set has 999,999 records and 6 fields.
# This will be used at the final stage to evaluated the model created using the edx data set



######################   Analyze frequency of ratings   ########################


# create table that describes total number ratings received per rating

number_of_rating <-
  edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  mutate(rating = factor(rating))

# plot rating vs count for that rating

number_of_rating %>%
  ggplot(aes(rating, count)) +
  geom_col() +
  labs(title = "Number of Rating")

# Plot ranking of rating

number_of_rating %>%
  ggplot(aes(reorder(rating,count), count)) +
  geom_col() +
  labs(title = "Number of Rating - Ranked")

######################   Analyze ratings by userId   ###########################

# Histogram of number of rating given per user

edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 20, color = "black") +
  scale_x_log10() +
  labs(title = "Ratings given per user",
       x = "Number of ratings from users")

# create table that describes rating activity of each user
user_summary <-
  edx %>% 
  group_by(userId) %>%
  summarize(user_number_rated = n(),
            user_mu = mean(rating),
            user_sd = sd(rating))

# show summary statistics of table created above describing rating activity of users
summary(user_summary)


# Top 10 users who has given the most ratings. The max is 6616 rankings by a user
user_summary %>%
  arrange(desc(user_number_rated)) %>%
  head(10)

# Bottom 10 users who has given the least ratings. The min is 10 rankings by a useer
user_summary %>%
  arrange(user_number_rated) %>%
  head(10)

# average number of ratings provider per user is 129
mean(user_summary$user_number_rated)

# plot number of rating given by user vs average rating given by that user
user_summary %>%
  ggplot(aes(user_number_rated, user_mu)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  labs(x = "number of rating given by user",
       y = "avg rating",
       title = "ratings given vs avg rating")


######################   Analyze ratings by movieId   ##########################


# histogram of number of rating per movie

edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 20, color = "black") +
  scale_x_log10() +
  labs(title = "Ratings received by movie",
       x = "Number of ratings movies received")

# create table that describes how often each moving is rated and its rating
movie_summary <-
  edx %>% 
  group_by(movieId) %>%
  summarize(movie_number_rated = n(),
            movie_mu = mean(rating),
            movie_sd = sd(rating))

# show summary statistics of table created above describing rating activity of movies
summary(movie_summary)

# Top 10 movies that have received the most ratings
movie_summary %>%
  arrange(desc(movie_number_rated)) %>%
  head(10)

# Bottom 10 movies that have received the least ratings
movie_summary %>%
  arrange(movie_number_rated) %>%
  head(10)

# average number of ratings provide per movie is 843
mean(movie_summary$movie_number_rated)

# plot number of rating recevied by movie and its average rating
movie_summary %>%
  ggplot(aes(movie_number_rated, movie_mu)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  labs(x = "number of rating recevied by movies",
       y = "avg rating",
       title = "ratings recieved vs avg rating")


################   Analyze ratings by release year   ###############


# create table that describes how movies perform for rating based on release year

release_year_summary <-
  edx %>%
  group_by(release_year) %>%
  summarize(release_year_number_of_rating  = n(),
            release_year_mu = mean(rating),
            release_year_sd = sd(rating))

# show summary statistics of table created above describing rating variation by release year
summary(release_year_summary)

# plot release year of movies vs number of ratings received by movies during that year
release_year_summary %>%
  ggplot(aes(release_year, release_year_number_of_rating)) +
  geom_point(alpha = 0.1) +
  geom_line() +
  labs(x = "release year",
       y = "number of ratings",
       title = "combined number of ratings received vs release yaer")

# plot release year of movies vs average ratings received by movies during that year
release_year_summary %>%
  ggplot(aes(release_year, release_year_mu)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  labs(x = "release year",
       y = "avg rating",
       title = "avg rating movies received vs release year")


########################   Analyze rating by genre   ###########################

# create table that describes how movies perform for rating based on genre

genre_summary <-
  edx %>%
  group_by(genres) %>%
  summarize(genres_number_of_rating  = n(),
            genre_mu = mean(rating),
            genre_sd = sd(rating))

# show summary statistics of table created above describing rating variation by genre
summary(genre_summary)

# display a table showing the most rated genres
genre_summary %>%
  arrange(desc(genres_number_of_rating)) %>%
  head(10)

# display a table showing the least rated genres
genre_summary %>%
  arrange(genres_number_of_rating) %>%
  head(10)

# display a table showing the most highly ranked genres
genre_summary %>%
  arrange(desc(genre_mu)) %>%
  head(10)

# display a table showing the worst ranked genres
genre_summary %>%
  arrange(genre_mu) %>%
  head(10)


##################################################
# Create recommendation models and calcualte RMSEs
##################################################

# create function for calculating root mean square error (RMSE)
RMSE <- function (true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

################# Model 1: based on just using the avg rating ################## 

# create a simple model that predicts average rating all the time
mu_hat <- mean(edx$rating)

# calculate RMSE of model
naive_rmse <- RMSE(validation$rating, mu_hat)

# load results to a table summarizing the various models
rmse_results <-
  data_frame(method = "Just the average", RMSE = naive_rmse)

# print out all the RMSEs for all the models explored above
rmse_results %>% knitr::kable()




################# Model 2: account for movie effect ############################ 

# store average rating in mu
mu <- mean(edx$rating)

# calculate movie effect and store result in table 
movie_avgs <- 
  edx %>%
  group_by(movieId) %>%
  summarize(b_movie = mean(rating - mu))

# predict rating of movies taking into account the above calculated movie effect
predicted_ratings_movie <-
  mu +
  validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  .$b_movie

# calculate RMSE of model
model_movie_rmse <- RMSE(validation$rating, predicted_ratings_movie)

# load results to a table summarizing the various models
rmse_results <-
  bind_rows(rmse_results,
            data_frame(method = "Movie Effect Model",
                       RMSE = model_movie_rmse))

# print out all the RMSEs for all the models explored above
rmse_results %>% knitr::kable()


####################### Model 3: account for user effect ####################### 

# calculate user effect and store result in table 
user_avgs <-
  edx %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_user = mean(rating - mu - b_movie))

# predict rating of movies taking into account the above calculated user effect
predicted_ratings_movie_user <-
  validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mu + b_movie + b_user) %>%
  .$pred

# calculate RMSE of model
model_movie_user_rmse <- RMSE(validation$rating, predicted_ratings_movie_user)

# load results to a table summarizing the various models
rmse_results <-
  bind_rows(rmse_results,
            data_frame(method = "Movie+User Effect Model",
                       RMSE = model_movie_user_rmse))

# print out all the RMSEs for all the models explored above
rmse_results %>% knitr::kable()

####################### Model 4: account for release year effect ###############

# calculate release year effect and store result in table
year_avgs <-
  edx %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  group_by(release_year) %>%
  summarize(b_year = mean(rating - mu - b_movie - b_user))

# predict rating of movies taking into account the above calculated release year effect
predicted_ratings_movie_user_year <-
  validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(year_avgs, by = 'release_year') %>%
  mutate(pred = mu + b_movie + b_user + b_year) %>%
  .$pred

# calculate RMSE of model
model_movie_user_year_rmse <- RMSE(validation$rating, predicted_ratings_movie_user_year)

# load results to a table summarizing the various models
rmse_results <-
  bind_rows(rmse_results,
            data_frame(method = "Movie+User+Year Effect Model",
                       RMSE = model_movie_user_year_rmse))

# print out all the RMSEs for all the models explored above
rmse_results %>% knitr::kable()


####################### Model 5: account for genre effect ######################

# calculate genre effect and store result in table
genre_avgs <-
  edx %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  group_by(genres) %>%
  summarize(b_genres = mean(rating - mu - b_movie - b_user))

# predict rating of movies taking into account the above calculated genre effect
predicted_ratings_movie_user_year_genres <-
  validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(year_avgs, by = 'release_year') %>%
  left_join(genre_avgs, by = 'genres') %>%
  mutate(pred = mu + b_movie + b_user + b_year+b_genres) %>%
  .$pred

# calculate RMSE of model
model_movie_user_year_genres_rmse <- RMSE(validation$rating, predicted_ratings_movie_user_year_genres)

# load results to a table summarizing the various models
rmse_results <-
  bind_rows(rmse_results,
            data_frame(method = "Movie+User+Year+genres Effect Model",
                       RMSE = model_movie_user_year_genres_rmse))

# print out all the RMSEs for all the models explored above
rmse_results %>% knitr::kable()

###### Model 6: regularized model with effects from user, movie, genre & year #######

# create a list of lambdas to explore
lambdas <- seq(0, 10, 0.25)

# calculate RMSEs of model with different lambdas
rmses <- sapply(lambdas, function(l) {
  b_movie_reg <-
    edx %>%
    group_by(movieId) %>%
    summarize(b_movie_reg = sum(rating - mu) / (n() + l))
  b_user_reg <-
    edx %>%
    left_join(b_movie_reg, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_user_reg = sum(rating - mu - b_movie_reg) / (n() + l))
  b_year_reg <-
    edx %>%
    left_join(b_movie_reg, by = "movieId") %>%
    left_join(b_user_reg, by = "userId") %>%
    group_by(release_year) %>%
    summarize(b_year_reg = sum(rating - mu - b_movie_reg - b_user_reg) / (n() + l))
  b_genres_reg <-
    edx %>%
    left_join(b_movie_reg, by = "movieId") %>%
    left_join(b_user_reg, by = "userId") %>%
    left_join(b_year_reg, by = "release_year") %>%
    group_by(genres) %>%
    summarize(b_genres_reg = sum(rating - mu - b_movie_reg - b_user_reg - b_year_reg) / (n() + l))
  predicted_reg_ratings_movie_user_year_genres <-
    validation %>%
    left_join(b_movie_reg, by = "movieId") %>%
    left_join(b_user_reg, by = "userId") %>%
    left_join(b_year_reg, by = "release_year") %>%
    left_join(b_genres_reg, by = "genres") %>%
    mutate(pred = mu + b_movie_reg + b_user_reg + b_year_reg + b_genres_reg) %>%
    .$pred
  model_reg_movie_user_year_genres_rmse <- RMSE(validation$rating, predicted_reg_ratings_movie_user_year_genres)
  return(model_reg_movie_user_year_genres_rmse)
})

# plot lambdas vs RMSEs to graphically display lambdas that lead to minimum RMSE
qplot(lambdas, rmses) + labs(title = "Lambda vs RMSE", x= "Lambda", y = "RMSE")

# store the best RMSE
lambda_min <- lambdas[which.min(rmses)]
rmses_min <- min(rmses)

# load results to a table summarizing the various models
rmse_results <-
  bind_rows(rmse_results,
            data_frame(method = "Movie+User+Year+genres Regularized Model",
                       RMSE = rmses_min))

# print out all the RMSEs for all the models explored
rmse_results %>% knitr::kable()
