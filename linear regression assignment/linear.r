
#Packages to be used:
#recommenderlab
#ggplot

#In this case study, we will look at :

#Loading movie lens package from recommenderlabs

library(recommenderlab)
library(ggplot2)

setwd("C:\\Users\\vshastri\\Downloads")

beer_original<-read.csv("beer_data.csv")
beer_df<-beer_original[,c(2,1,3)]
beer_df<-beer_df[-c(which(beer_df$review_profilename=='')),]
freq_beer_id<-as.data.frame(table(beer_df$beer_beerid))
beer_considered<-as.integer(freq_beer_id[which(freq_beer_id$Freq>70),]$Var1)
beer_df<-beer_df[beer_df$beer_beerid %in% beer_considered,]
beer <- as(beer_df, "realRatingMatrix")

beer_coerced<-as(beer,"data.frame")

# get some informtaion
#dimnames(beer)
#rowCounts(beer)
#colCounts(beer)
#rowMeans(beer)

#How similar are the first ten users are with each other
similar_users <- similarity(beer[1:10, ],
                            method = "cosine",
                            which = "users")

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

#Inference
#Users 7 and 8 are similar

#How similar are the first five items are with each other

similar_items <- similarity(beer[,1:10 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_items)

image(as.matrix(similar_items), main = "Item similarity")

# items 3,4,5,6,8,9,10 are similar;

#--------------------------Understand users and ratings----------#

# Visualizing ratings

qplot(getRatings(beer), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")

summary(getRatings(beer)) # Skewed to the right

qplot(getRatings(normalize(beer, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "Rating") 

summary(getRatings(normalize(beer, method = "Z-score"))) # seems better


qplot(rowCounts(beer), binwidth = 10, 
      main = "Beers Rated on average", 
      xlab = "# of users", 
      ylab = "# of beers rated")
#Most users rate less number of beers
#Very few users have rated more beers

#--------------------------Recommendation models ----------------#

#let's compare user based and item based
#collaborative filtering

#Divide data into test 
scheme_split <- evaluationScheme(beer, method = "split", train = .9,
                           k = 1, given = 1, goodRating = 4)
scheme_cv <- evaluationScheme(beer, method = "cross-validation", train = .9,
                           k = 1, given = 1, goodRating = 4)

scheme_split
scheme_cv

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# run algorithms, predict next n movies
results_split <- evaluate(scheme_split, algorithms, n=c(1, 3, 5, 10, 15, 20))
result_cv<- evaluate(scheme_cv, algorithms, n=c(1, 3, 5, 10, 15, 20))
#we see that the recommender with scheme_cv is failing.

# Draw ROC curve
plot(results_split, annotate = 1:4, legend="topleft")

#We can see user based collaborative filtering is better in this case.
rec<-Recommender(beer,method="IBCF")
p_cokes<-predict(rec,beer["cokes"],n=5)
as(p_cokes, "list")
# [1] "68"   "161"  "508"  "1326" "1434"

p_genog<-predict(rec,beer["genog"],n=5)
as(p_genog, "list")
# [1] "68"   "126"  "1772" "1877" "2122"

p_giblet<-predict(rec,beer["giblet"],n=5)
as(p_giblet, "list")
# [1] "5"   "67"  "85"  "86"  "114"

