#Sub: Recsys-001 Assigment#1 
#Author: Chaitra Kamath Kothari
#Description: The problem assignment required to do some analysis on given data manually
#or using excel. However, I have translated them to R code here. 

#Read the data and view it
rm(list = ls())
ratings <- read.csv('/Users/admin/Documents/Coursera/IntroToRecommenderSystems/Assignments/as-1/A1Ratings.csv')
str(ratings)

#Part1: Find mean ratings of for each movie
meanRatings <- as.data.frame(colMeans(ratings[, -1], na.rm = TRUE))
meanRatingsSub <- data.frame (movieID = rownames(meanRatings), meanRatings = meanRatings[[1]])
meanRatingsSub <- meanRatingsSub[order(meanRatingsSub$meanRatings, decreasing = TRUE), ]
meanRatingsSub

#Part2: Find proportion of 4+ ratings for each movie
ratingsub <- as.data.frame(sapply(colnames(ratings)[-1], function(x) as.vector(table(ratings[!is.na(ratings[, x]), x] >= 4)[2])) / 
                                   sapply(colnames(ratings)[-1], function(x) length(which(!is.na(ratings[, x])))))
proportiondf <- data.frame(movieID = rownames(ratingsub), proportion = ratingsub[[1]])
proportiondf <- proportiondf[order(proportiondf$proportion, decreasing = TRUE), ]
proportiondf

#Part3: Find number of ratings for each movie
count <- as.data.frame(sapply(colnames(ratings)[-1], function(x) length(which(!is.na(ratings[, x])))))
countdf <- data.frame(movieID = rownames(count), count = count[[1]])
countdf <- countdf[order(countdf$count, decreasing = TRUE), ]
countdf

#Part4: Find proportion of ratings that are common between each movie and Star Wars Episode IV
starwarsSub <- ratings[complete.cases(ratings$X260..Star.Wars..Episode.IV...A.New.Hope..1977.), ]
otherratings <- as.data.frame(sapply(colnames(starwarsSub[, -c(1, 2)]), function(x) length(which(!is.na(starwarsSub[, x])))))
otherratingsdf <- data.frame(movieID = rownames(otherratings), x_y = otherratings[[1]])
swAndOthers <- data.frame(movieID = as.character(otherratingsdf$movieID), condprob = as.numeric(otherratingsdf$x_y / 15))
swAndOthers <- swAndOthers[order(swAndOthers$condprob, decreasing = TRUE), ]
swAndOthers

