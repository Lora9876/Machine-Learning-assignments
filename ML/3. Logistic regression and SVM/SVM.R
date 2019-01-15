library(e1071)

dataset <- read.csv('movie_metadata.csv', header = TRUE)
dataset <- na.omit(dataset)
set.seed(100)
sapply(dataset, function(x) sum(is.na(x)))

training<-dataset[which(dataset$title_year>1999 & dataset$title_year<2015),]
test<-dataset[which(dataset$title_year>=2015),]

model <- svm(training$imdb_score ~ duration+num_critic_for_reviews+budget+cast_total_facebook_likes+movie_facebook_likes, training)
fit <- predict(model, test)
error <- training$imdb_score - fit
rmse <- function(error){
  sqrt(mean(error^2))
}

rmse(error)
library(caret)

x <- seq(1:75)
{plot(x,test[1:75,]$imdb_score,type = 'l', col='purple', xlab = '', ylab = 'score')
  lines(x,fit[1:75], col='blue')
  legend("topright", legend = c('Test data', 'Predicted data'), col = c('purple', 'blue'), lty=1, cex=.75)}
