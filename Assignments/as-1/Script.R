rm(list = ls())
ratings <- read.csv('/Users/admin/Documents/Coursera/IntroToRecommenderSystems/Assignments/as-1/A1Ratings.csv')
str(ratings)

meanRatings <- as.data.frame(colMeans(ratings[, -1], na.rm = TRUE))
meanRatingsSub <- data.frame (movieID = rownames(meanRatings), meanRatings = meanRatings[[1]])
meanRatingsSub <- meanRatingsSub[order(meanRatingsSub$meanRatings, decreasing = TRUE), ]
meanRatingsSub


ratingsub <- as.data.frame(sapply(colnames(ratings)[-1], function(x) as.vector(table(ratings[!is.na(ratings[, x]), x] >= 4)[2])) / 
        sapply(colnames(ratings)[-1], function(x) length(which(!is.na(ratings[, x])))))
proportiondf <- data.frame(movieID = rownames(ratingsub), proportion = ratingsub[[1]])
proportiondf <- proportiondf[order(proportiondf$proportion, decreasing = TRUE), ]
proportiondf

count <- as.data.frame(sapply(colnames(ratings)[-1], function(x) length(which(!is.na(ratings[, x])))))
countdf <- data.frame(movieID = rownames(count), count = count[[1]])
countdf <- countdf[order(countdf$count, decreasing = TRUE), ]
countdf

starwarsSub <- ratings[complete.cases(ratings$X260..Star.Wars..Episode.IV...A.New.Hope..1977.), ]
otherratings <- as.data.frame(sapply(colnames(starwarsSub[, -c(1, 2)]), function(x) length(which(!is.na(starwarsSub[, x])))))
otherratingsdf <- data.frame(movieID = rownames(otherratings), x_y = otherratings[[1]])
swAndOthers <- data.frame(movieID = as.character(otherratingsdf$movieID), condprob = as.numeric(otherratingsdf$x_y / 15))
swAndOthers <- swAndOthers[order(swAndOthers$condprob, decreasing = TRUE), ]
swAndOthers
