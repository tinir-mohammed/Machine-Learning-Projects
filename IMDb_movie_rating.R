library(rpart)
library(dplyr)
library(tidyverse)
library(caTools)
library(GGally)
library(caret)
library(rpart.plot)
library(visdat)
library(naniar)
library(plotly)

# Reading the dataset
imdbdat <- read.csv("movie_metadata.csv", stringsAsFactors = FALSE, as.is=TRUE)

# Structure of the dataset
str(imdbdat)

# Head of the dataset, i.e. first six observations
head(imdbdat)

# Trimming surrounding whitespaces in all character columns
for (i in 1:length(imdbdat)) {
  imdbdat[i] <- imdbdat[i] %>% mutate_if(is.character, function(x) str_trim(x))
}

# The character Â appears at the end of movie titles when opened in Microsoft Excel
# Removing the character Â
imdbdat$movie_title <- imdbdat$movie_title %>% str_replace_all("Â", "")
head(imdbdat)

# Finding the unique genres
allgenres <- unlist(strsplit(imdbdat$genres, "\\|"))
table(allgenres)
sort(table(allgenres), decreasing = TRUE)
allgenres <- names(sort(table(allgenres), decreasing = TRUE))
allgenres <- allgenres[1:20]
allgenres

# GENRE: Determining if genre has any meaningful bearing on score
# Creating a dataframe with only genres and scores
genrescore <- as.data.frame(imdbdat[,c("genres", "imdb_score")])

# Widen the table by adding logical column for each genre
genrescore[allgenres] <- FALSE
for (i in 1:length(allgenres)) {
  for (j in 1:nrow(genrescore)) {
    genrescore[j, 2+i] <- str_detect(genrescore[j, 1], allgenres[i])
  }
}
head(genrescore)

# Finding the mean rating for each genre
genrescoremeans <- rep(0, length(allgenres))
for (i in 1:length(allgenres)) {
  genrescoremeans[i] <- mean(genrescore$imdb_score[genrescore[2+i] == TRUE])
}
##### DATA VISUALIZATION ##### 
# Plotting a bar graph of ratings and genres
barplot(genrescoremeans, main="Mean IMDb scores by genre", xlab="Genre", ylab="Mean IMDb Score")
# The barplot shows that genres do not have much of an influence on the IMDb score of a film, hence we can disregard genres as a feature

# Datatype visualization
vis_dat(imdbdat)

# Missing values vizualization
vis_miss(imdbdat)
gg_miss_var(imdbdat)
gg_miss_upset(imdbdat)

# Scatterplot of movie_facebook_likes vs imdb_score
imdbdat %>% plot_ly(x = ~movie_facebook_likes, y = ~imdb_score, mode = "markers", alpha = 0.7, type = "scatter")

# Scatterplot of profit vs imdb_score
imdbdat$profit <- (imdbdat$gross - imdbdat$budget)
imdbdat %>% plot_ly(x = ~profit, y = ~imdb_score, mode = "markers", alpha = 0.7, type = "scatter")

# PLOT_KEYWORDS: Finding the unique plot keywords
plotkeys <- unlist(strsplit(imdbdat$plot_keywords, "\\|"))
plotkeys <- unique(plotkeys)
length(plotkeys)
head(plotkeys)
# There are too many plot keywords to have any meaningful influence on the score, hence we can also disregard plot_keywords

# Number of missing values (NA's) in each column
colSums(sapply(imdbdat, is.na))

# We see that aspect ration has the highest frequency of missing values
table(imdbdat$aspect_ratio)

# We also see that budget and gross have many NA's
imdbdat <- imdbdat[!is.na(imdbdat$gross), ]
imdbdat <- imdbdat[!is.na(imdbdat$budget), ]
dim(imdbdat)

# Replacing NA with 0 first before computing mean
imdbdat$aspect_ratio[is.na(imdbdat$aspect_ratio)] <- 0
mean(imdbdat$imdb_score[imdbdat$aspect_ratio == 1.85])

# Observe the mean to see if removing it affects the rating or not
mean(imdbdat$imdb_score[imdbdat$aspect_ratio == 2.35])
mean(imdbdat$imdb_score[imdbdat$aspect_ratio != 1.85 & imdbdat$aspect_ratio != 2.35])

# From the means of imdb score for different aspect ratios, we can see there is no significant difference, all the means fall in the range of 6.3~6.8
imdbdat <- subset(imdbdat, select = -c(aspect_ratio))

# Replacing NA with column average for facenumber_in_poster
imdbdat$facenumber_in_poster[is.na(imdbdat$facenumber_in_poster)] <- round(mean(imdbdat$facenumber_in_poster, na.rm = TRUE))

# Converting 0's into NAs for predictors other than facenumber_in_poster
imdbdat[,c(5,6,8,13,24,26)][imdbdat[,c(5,6,8,13,24,26)] == 0] <- NA

# Now that all the 0 values are NAs, we impute by using the column means for all
# 1. num_critic_for_reviews
imdbdat$num_critic_for_reviews[is.na(imdbdat$num_critic_for_reviews)] <- round(mean(imdbdat$num_critic_for_reviews, na.rm = TRUE))

# 2. duration
imdbdat$duration[is.na(imdbdat$duration)] <- round(mean(imdbdat$duration, na.rm = TRUE))

# 3. director_facebook_likes
imdbdat$director_facebook_likes[is.na(imdbdat$director_facebook_likes)] <- round(mean(imdbdat$director_facebook_likes, na.rm = TRUE))

# 4. actor_3_facebook_likes
imdbdat$actor_3_facebook_likes[is.na(imdbdat$actor_3_facebook_likes)] <- round(mean(imdbdat$actor_3_facebook_likes, na.rm = TRUE))

# 5. actor_1_facebook_likes
imdbdat$actor_1_facebook_likes[is.na(imdbdat$actor_1_facebook_likes)] <- round(mean(imdbdat$actor_1_facebook_likes, na.rm = TRUE))

# 6. cast_total_facebook_likes
imdbdat$cast_total_facebook_likes[is.na(imdbdat$cast_total_facebook_likes)] <- round(mean(imdbdat$cast_total_facebook_likes, na.rm = TRUE))

# 7. actor_2_facebook_likes
imdbdat$actor_2_facebook_likes[is.na(imdbdat$actor_2_facebook_likes)] <- round(mean(imdbdat$actor_2_facebook_likes, na.rm = TRUE))

# 8. movie_facebook_likes
imdbdat$movie_facebook_likes[is.na(imdbdat$movie_facebook_likes)] <- round(mean(imdbdat$movie_facebook_likes, na.rm = TRUE))

# REMOVAL OF COLUMNS
# Removing colour of movies, because it seems insignificant: 96% are coloured, i.e. colour is almost constant
table(imdbdat$color)
imdbdat <- subset(imdbdat, select = -c(color))

# Removing language of movies, because it seems insignificant: Over 95% movies are in English, i.e. langauge is nearly constant

table(imdbdat$language)
imdbdat <- subset(imdbdat, select = -c(language))

# Modifying countries to a categorical variable with fewer levels: UK, USA, and others
# Because around 79% movies are from USA, 8% from UK, 13% from other countries.

levels(imdbdat$country) <- c(levels(imdbdat$country), "Others")
imdbdat$country[(imdbdat$country != 'USA')&(imdbdat$country != 'UK')] <- 'Others'
imdbdat$country <- factor(imdbdat$country)
table(imdbdat$country)

# Removing content rating column, because it seems insignificant: Over 70% of the data are PG-13 and R there are only a few data from other rates.

table(imdbdat$content_rating)
imdbdat <- subset(imdbdat, select = -c(content_rating))

# Removing director name column, because they are too many directors to be have any meaningful impact on score

length(unique(imdbdat$director_name))
imdbdat <- subset(imdbdat, select = -c(director_name))

# Removing plot keywords, as discussed above
imdbdat <- subset(imdbdat, select = -c(plot_keywords))

# Removing genre, as discussed above
imdbdat <- subset(imdbdat, select = -c(genres))

# Removing face-count on poster, because almost 90% have a count from 0-3 over 20% have 4-15 which seems insignificant to predict rating.
table(imdbdat$facenumber_in_poster)
imdbdat <- subset(imdbdat, select = -c(facenumber_in_poster))

# Removing movie_imdb_link because it is irrelevant
imdbdat <- subset(imdbdat, select = -c(movie_imdb_link))

# Removing actor names because they are too numerous
# c(imdbdat$actor_3_name, imdbdat$actor_2_name, imdbdat$actor_1_name)
length(unique(c(imdbdat$actor_3_name, imdbdat$actor_2_name, imdbdat$actor_1_name)))
imdbdat <- subset(imdbdat, select = -c(actor_1_name, actor_2_name, actor_3_name))


# Removing the title column because it is a character column and they are too diverse
imdbdat <- imdbdat[,-7]
str(imdbdat)

# Removing budget outliers before normalizing
boxplot(imdbdat$budget)
summary(imdbdat$budget)
imdbdat <- imdbdat %>% filter(budget <= 500000000)
boxplot(imdbdat$budget)

# Normalizing all the numerical columns
for (i in colnames(imdbdat)) {
  if ((class(imdbdat[[i]]) == "integer" || class(imdbdat[[i]]) == "numeric") && i != "title_year" && i != "imdb_score") {
    # print(imdbdat[[i]])
    mn <- min(imdbdat[[i]])
    mx <- max(imdbdat[[i]])
    imdbdat[[i]] <- (imdbdat[[i]] - mn) / (mx - mn)
  }
}
#Data Viz after removing outliers and normalization
# Plotting gross and gross against budget
imdbdat %>% plot_ly(x = ~budget, y = ~gross, mode = "markers", alpha = 0.7, type = "scatter")
# Plotting gross and profit against budget
imdbdat %>% plot_ly(x = ~budget, y = ~profit, mode = "markers", alpha = 0.7, type = "scatter")
# Plotting gross and imdb_score against movie_facebook_likes
imdbdat %>% plot_ly(x = ~movie_facebook_likes, y = ~imdb_score, mode = "markers", alpha = 0.7, type = "scatter")
# Plotting gross and imdb_score against profit
imdbdat %>% plot_ly(x = ~profit, y = ~imdb_score, mode = "markers", alpha = 0.7, type = "scatter")


# Current correlation heatmap
ggcorr(imdbdat, label = TRUE, label_alpha = TRUE)

# Adding up the likes for three lead actors and entire cast (high correlation)
imdbdat$actor_facebook_likes <- imdbdat$actor_1_facebook_likes + imdbdat$actor_2_facebook_likes + imdbdat$actor_3_facebook_likes + imdbdat$cast_total_facebook_likes
imdbdat <- subset(imdbdat, select = -c(actor_1_facebook_likes, actor_2_facebook_likes, actor_3_facebook_likes, cast_total_facebook_likes))

# Final structure of the dataset
str(imdbdat)

# Final correlation heatmap
ggcorr(imdbdat, label = TRUE, label_alpha = TRUE)

##### TRAINING AND TESTING #####
# Splitting the dataset into training (80%) and testing (20%) sets
set.seed(123)
sample = sample.split(imdbdat, SplitRatio = 0.80)
trainset = subset(imdbdat, sample == TRUE)
testset = subset(imdbdat, sample == FALSE)

# Training the model
cpydat <- trainset
cpydat$pred <- mean(trainset$imdb_score)
cpydat$resid <- cpydat$imdb_score - cpydat$pred
alphaval <- 0.1
treel <- list() #Will have the sequence of regression decision trees
prevloss <- sum(cpydat$resid * cpydat$resid)
for (i in 1:1000) {
  # print(prevloss)
  temptree <- rpart(resid~.-pred-imdb_score, data=cpydat, method="anova")
  ress <- predict(temptree, cpydat)
  temppred <- (cpydat$pred + alphaval * ress)
  newloss <- sum((cpydat$imdb_score - temppred)^2)
  if (newloss < prevloss) {
    treel[[i]] <- temptree
    cpydat$pred <- temppred
    cpydat$resid <- cpydat$imdb_score - cpydat$pred
    prevloss <- newloss
  }
  else {
    break
  }
}

# Printing minimum loss and number of trees
cat("Minimum loss obtained =", prevloss, "\n")
cat("Number of trees =", i, "\n")

i <- i - 1

# Testing the model
testset$pred <- mean(testset$imdb_score)
for (i in 1:i) {
  ress <- predict(treel[[i]], testset)
  testset$pred <- testset$pred + alphaval * ress
}

# Printing sample of actual and predicted values
predvsact <- data.frame(actualScore = testset$imdb_score, predictedScore = testset$pred)
cat("Sample of actual and predicted IMDb scores")
print(head(predvsact))

# Evaluation metrics
sse <- sum ((testset$imdb_score - testset$pred)^2)
rmse <- sqrt(sum((testset$imdb_score - testset$pred)^2 / n))
sstot <- sum((testset$imdb_score - mean(testset$imdb_score))^2)
rsq <- 1 - (sse / sstot)
mae <- sum(abs(testset$pred - testset$imdb_score)) / n
numtrees <- i + 1

evalMetrics <- data.frame(noOfTrees = numtrees, SSE = sse, RMSE = rmse, rSquared = rsq, MAE = mae)
print(evalMetrics)

avgsd <- data.frame(score = c("Actual", "Predicted"), average = c(mean(testset$imdb_score), mean(testset$pred)), stdDev = c(sd(testset$imdb_score), sd(testset$pred)))
print(avgsd)

# Printing the last regression tree in the tree
prp(treel[[10]])

###### CROSS VALIDATION ######
# Set initial seed so experiment can be replicated
# set.seed(123)

# Shuffling all the rows of the dataset
cpydat <- imdbdat[sample(nrow(imdbdat)),]

# Creating k folds
k <- 10
folds <- cut(seq(1, nrow(cpydat)), breaks=k, labels=FALSE)

# Creating vectors to store evaluation metrics for each fold
numtrees <- vector()
sse <- vector()
rmse <- vector()
rsq <- vector()
mae <- vector()
sdact <- vector()
sdpred <- vector()
avgact <- vector()
avgpred <-vector()

# k-fold cross validation
for (i in 1:k) {
  testind <- which(folds == i)
  testset <- cpydat[testind,]
  trainset <- cpydat[-testind,]
  
  # Training the model
  trainset$pred <- mean(trainset$imdb_score)
  trainset$resid <- trainset$imdb_score - trainset$pred
  alphaval <- 0.1
  treell <- list() #Will have the sequence of regression decision trees
  prevloss <- sum(trainset$resid * trainset$resid)
  for (j in 1:1000) {
    # print(prevloss)
    temptree <- rpart(resid~.-pred-imdb_score, data=trainset, method="anova")
    ress <- predict(temptree, trainset)
    temppred <- (trainset$pred + alphaval * ress)
    x <- trainset$imdb_score - temppred
    newloss <- sum(x * x)
    if (newloss < prevloss) {
      treell[[j]] <- temptree
      trainset$pred <- temppred
      trainset$resid <- trainset$imdb_score - trainset$pred
      prevloss <- newloss
    }
    else {
      break
    }
  }
  j <- j - 1

  # Testing the model
  testset$pred <- mean(testset$imdb_score)
  for (x in 1:j) {
    ress <- predict(treell[[x]], testset)
    testset$pred <- testset$pred + alphaval * ress
  }
  
  # Calculating evaluation metrics
  n <- nrow(testset)
  
  sse[i] <- sum ((testset$imdb_score - testset$pred)^2)
  rmse[i] <- sqrt(sum((testset$imdb_score - testset$pred)^2 / n))
  ssres <- sse[i]
  sstot <- sum((testset$imdb_score - mean(testset$imdb_score))^2)
  rsq[i] <- 1 - (ssres / sstot)
  mae[i] <- sum(abs(testset$pred - testset$imdb_score)) / n
  numtrees[i] <- j+1
  sdact[i] <- sd(testset$imdb_score)
  sdpred[i] <- sd(testset$pred)
  avgact[i] <- mean(testset$imdb_score)
  avgpred[i] <- mean(testset$pred)
  
  # Printing predicted values and actual values
  predvact <- data.frame(actualScore = testset$imdb_score, predictedScore = testset$pred)
  cat("Fold ", i, ": Sample of actual and predicted IMDb score\n", sep = "")
  print (head(predvact))
}

# Printing evaluation metrics
evalMetrics <- data.frame(kFold = 1:k, noOfTrees = numtrees, SSE = sse, RMSE = rmse, rSquared = rsq, MAE = mae)
print("Evaluation metrics", quote = FALSE)
print(evalMetrics)
cat("Average over", k, "folds\n")
colMeans(evalMetrics[,2:6])

avgsd <- data.frame(score = c("Actual", "Predicted"), average = c(mean(avgact), mean(avgpred)), stdDev = c(mean(sdact), mean(sdpred)))
print(avgsd)

# Printing the last regression tree of model from last cross-valudation iteration
prp(treell[[j]])

