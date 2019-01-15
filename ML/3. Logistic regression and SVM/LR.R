dataset <- read.csv('movie_metadata.csv', header = TRUE, stringsAsFactors = FALSE)
summary(dataset)
str(dataset)
sapply(dataset, function(x) sum(is.na(x)))

dataset$imdb_score =as.factor(ifelse (dataset$imdb_score>=6,1,0))
training<-dataset[which(dataset$title_year>2004 & dataset$title_year<2016),]
test<-dataset[which(dataset$title_year==2016),]

model <- glm(imdb_score~duration+num_critic_for_reviews+budget+cast_total_facebook_likes+movie_facebook_likes, data=training, family=binomial(link=logit))
summary(model)

# calculate predictions and compare to test, returning accuracy
results <- predict(model, test, type='response')
results.round <- ifelse(results >= 0.5, 1, 0)
mean(results.round == test$imdb_score)
summary(results.round)

pred <- prediction(results, test$imdb_score)
auc <- performance(pred, measure="auc")
auc@y.values[[1]]


#analyzing of the data, intentionaly moved into the end of the code

p1<-ggplot(dataset,aes(x=title_year))+ ggtitle("Year") + xlab("Year") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=duration))+ ggtitle("Duration") + xlab("Duration") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=movie_facebook_likes))+ ggtitle("FB likes") + xlab("FB likes") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=director_name))+ ggtitle("Director") + xlab("Director") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=actor_2_facebook_likes))+ ggtitle("Actor 2 likes") + xlab("Actor 2 likes") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=num_critic_for_reviews))+ ggtitle("Num critics") + xlab("Num critics") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=director_facebook_likes))+ ggtitle("Director likes") + xlab("Director likes") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=color))+ ggtitle("Color") + xlab("Color") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=actor_3_facebook_likes))+ ggtitle("Actor 3 likes") + xlab("Actor 3") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=actor_2_name))+ ggtitle("Actor 2 name") + xlab("Actor 2") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=actor_1_facebook_likes))+ ggtitle("Actor 1 likes") + xlab("Actor 1 likes") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=gross))+ ggtitle("Gross") + xlab("gross") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=genres))+ ggtitle("Genres") + xlab("genres") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=actor_1_name))+ ggtitle("Actor 1") + xlab("Actor 1 ") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=movie_title))+ ggtitle("Movie title") + xlab("Movie title") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=num_voted_users))+ ggtitle("Num voted users") + xlab("num vored users") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=cast_total_facebook_likes))+ ggtitle("Cast likes") + xlab("Cast likes") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=actor_3_name))+ ggtitle("Actor 3") + xlab("Actor 3") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=facenumber_in_poster))+ ggtitle("Facenumber") + xlab("Facenumber") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=num_user_for_reviews))+ ggtitle("num_user_for_reviews") + xlab("num_user_for_reviews") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=language))+ ggtitle("Language") + xlab("Language") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=country))+ ggtitle("Country") + xlab("Country") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=content_rating))+ ggtitle("Rating") + xlab("Rating") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
p1<-ggplot(dataset,aes(x=budget))+ ggtitle("Budget") + xlab("Budzet") + geom_bar(aes(y=100*(..count..)/sum(..count..)), width=0.5)+ ylab("Percentage")+ coord_flip() + theme_minimal()
p1
