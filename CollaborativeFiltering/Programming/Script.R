rm(list = ls())
ratings <- read.csv('/Users/admin/Documents/Coursera/IntroToRecommenderSystems/CollaborativeFiltering/Programming/Assignment 3-Prog.csv')
str(ratings)
head(ratings)

#-------------------------------------Part1------------------------

#Compute correlations between users
library(data.table)
correlations <- cor(ratings[, - 1], use="pairwise.complete.obs")

#Find nearest 5 neighbors based on correlation scores
nearestNeighbors <- sapply(colnames(correlations), function(x){
        names(correlations[, x])[order(correlations[, x], decreasing=TRUE)][2:6]
        })
nnsorted <- nearestNeighbors[, order(colnames(nearestNeighbors))]
top5neighbors <- cbind.data.frame(user = sort(rep(colnames(nnsorted), 5)), neighbors = as.vector(nnsorted))
top5neighbors$correlations <- sapply(as.numeric(rownames(top5neighbors)), 
                         function(x){correlations[as.character(top5neighbors[x, 'user']), 
                         as.character(top5neighbors[x, 'neighbors'])]})

#Extract movie ratings for all 5 neighbors for each user
predRatings <- data.frame(matrix(NA, nrow = 100, ncol = 26))
predRatings[, 1] <- ratings$X
colnames(predRatings) <- colnames(ratings)

predRatings[, 'X1648'] <- rowSums(data.frame(mapply('*', ratings[, colnames(ratings) %in% 
as.character(top5neighbors[top5neighbors$user == 'X1648', 'neighbors']) ], 
top5neighbors[top5neighbors$user == 'X1648', 'correlations'], SIMPLIFY = FALSE)), na.rm = TRUE) / 
sum(top5neighbors[top5neighbors$user == 'X1648', 'correlations'])

#ratings [is.na(ratings)] <- 0
predictedRatings <- sapply(colnames(predRatings[, - 1]), function(x){
        predRatings[, x] <- rowSums(data.frame(mapply('*', ratings[, colnames(ratings) %in% 
                        as.character(top5neighbors[top5neighbors$user == x, 'neighbors']) ], 
                        top5neighbors[top5neighbors$user == x, 'correlations'], SIMPLIFY = FALSE)), 
                        na.rm = TRUE) / sum(top5neighbors[top5neighbors$user == x, 'correlations'])
        })

finalRatings <- cbind.data.frame(movieName = as.character(predRatings[, 1]), predictedRatings)



#----------------------------------Discussion Forum--------------
top5neighbors[top5neighbors$user == 'X3712', ]

ratings[, 'X2824']
ratings[, 'X3867']
ratings[, 'X5062']
ratings[, 'X442']
ratings[, 'X3853']

r1 <-  ratings[, 'X2824'] * top5neighbors[top5neighbors$user == 'X3712' & top5neighbors$neighbor == 'X2824','correlations' ]
r2 <-  ratings[, 'X3867']  * top5neighbors[top5neighbors$user == 'X3712' & top5neighbors$neighbor == 'X3867','correlations' ]
r3 <-  ratings[, 'X5062'] * top5neighbors[top5neighbors$user == 'X3712' & top5neighbors$neighbor == 'X5062','correlations' ]
r4 <-  ratings[, 'X442'] * top5neighbors[top5neighbors$user == 'X3712' & top5neighbors$neighbor == 'X442','correlations' ]
r5 <-  ratings[, 'X3853'] * top5neighbors[top5neighbors$user == 'X3712' & top5neighbors$neighbor == 'X3853','correlations' ]

num <- rowSums(cbind(r1, r2, r3, r4, r5), na.rm = TRUE)
den <- sum(top5neighbors[top5neighbors$user == 'X3712', 'correlations'])

predictions <- cbind.data.frame(movie = ratings$X, ratings = num / den)
#-------------------------------------Part2------------------------
#



