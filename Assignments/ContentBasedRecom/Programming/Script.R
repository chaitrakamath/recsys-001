rm(list = ls())
data <- read.csv('/Users/admin/Documents/Coursera/IntroToRecommenderSystems/Assignments/ContentBasedRecom/Programming/Assignment 2.csv')
data <- data[1:20, ]
data$X <- droplevels(data$X)
data$X <- factor(data$X, levels = data$X)
data[is.na(data$User.1), 'User.1'] = 0
data[is.na(data$User.2), 'User.2'] = 0
str(data) 

#Part 1. Build and use a very basic profile

user1_profile_part1 <- sapply(colnames(data[, -c(1, 12, 13)]), function(x) {data[, x] %*% data[, 'User.1']})
user1_predScores_part1 <- sapply(rownames(data[, -c(1, 12, 13)]), function(x)
        {crossprod(as.numeric(user1_profile_part1), 
        as.numeric(data[x, -c(1, 12, 13)]))})
user1_pred_part1 <- data.frame(X = data$X, Pred1 = user1_predScores_part1)
user1_pred_part1 <- user1_pred_part1[order(user1_pred_part1$Pred1, decreasing = TRUE),]
user1_pred_part1


user2_profile_part1 <- sapply(colnames(data[, -c(1, 12, 13)]), function(x) {data[, x] %*% data[, 'User.2']})
user2_predScores_part1 <- sapply(rownames(data[, -c(1, 12, 13)]), function(x)
                    {crossprod(as.numeric(user2_profile_part1), 
                    as.numeric(data[x, -c(1, 12, 13)]))})
user2_pred_part1 <- data.frame(X = data$X, Pred2 = user2_predScores_part1)
user2_pred_part1 <- user2_pred_part1[order(user2_pred_part1$Pred2, decreasing = TRUE),]
user2_pred_part1

#Part 2. 
data$numAttrs <- as.numeric(sapply(rownames(data[, -c(1, 12, 13)]), 
                function(x) rowSums(data[x, -c(1, 12, 13)])))
normalizeddf <- sapply(colnames(data[, -c(1, 12, 13, 14)]), 
                function(x){data[, x] / sqrt(data[, 'numAttrs'])})
normdata <- cbind.data.frame(X = levels(data$X), normalizeddf, User.1 = data$User.1, User.2 = data$User.2, 
                  numAttrs = data$numAttrs)


user1_profile_part2 <- sapply(colnames(normdata[, -c(1, 12, 13, 14)]), function(x) {normdata[, x] %*% normdata[, 'User.1']})
user1_predScores_part2 <- sapply(rownames(normdata[, -c(1, 12, 13, 14)]), 
                    function(x)
                   {
                   crossprod(as.numeric(user1_profile_part2), 
                   as.numeric(normdata[x, -c(1, 12, 13, 14)]))
                   })
user1_pred_part2 <- cbind.data.frame(X = normdata$X, Pred1 = user1_predScores_part2)
user1_predRes_part2 <- user1_pred_part2[order(user1_pred_part2$Pred1, decreasing = TRUE),]
user1_predRes_part2

user2_profile_part2 <- sapply(colnames(normdata[, -c(1, 12, 13, 14)]), function(x) {normdata[, x] %*% normdata[, 'User.2']})
user2_predScores_part2 <- sapply(rownames(normdata[, -c(1, 12, 13, 14)]), 
                           function(x)
                           {
                                   crossprod(as.numeric(user2_profile_part2), 
                                             as.numeric(normdata[x, -c(1, 12, 13, 14)]))
                           })
user2_pred_part2 <- cbind.data.frame(X = normdata$X, Pred2 = user2_predScores_part2)
user2_predRes_part2 <- user2_pred_part2[order(user2_pred_part2$Pred2, decreasing = TRUE),]
user2_predRes_part2

#Part 3. 
DF <-  colSums(data[, c(2:11)])
IDF <- 1 / DF

user1_predScores_part3 <- sapply(rownames(normdata[, c(2:11)]), function(x) {sum(user1_profile_part2 * normdata[x, c(2:11) ] * IDF)})
user1_pred_part3 <- cbind.data.frame(X = data$X, Pred1 = user1_predScores_part3)
user1_pred_part3

user2_predScores_part3 <- sapply(rownames(normdata[, c(2:11)]), function(x) {sum(user2_profile_part2 * normdata[x, c(2:11) ] * IDF)})
user2_pred_part3 <- cbind.data.frame(X = data$X, Pred2 = user2_predScores_part3)
user2_pred_part3
