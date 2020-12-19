library(rpart)
library(rpart.plot)
library(caret)
library(tidyr)

#summary(mal_df)
#'type', 'source', 'episodes', 'status', 'airing',
#       'aired_string', 'aired', 'duration', 'rating', 'score', 'scored_by',
#       'rank', 'popularity', 'members', 'favorites', 'premiered', 'producer',
#       'licensor', 'studio', 'genre'

mal_df = read.csv("AnimeList.csv", stringsAsFactors=TRUE)

#create column for premiered_year, premiered_season, and length of show in months
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon } 
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

mal_df["aired_from"] <- substr(mal_df[,"aired"], 11, 20)
mal_df["aired_to"] <- substr(mal_df[,"aired"], 31, 40)
mal_df["show_duration_months"] <- mondf(mal_df[,"aired_from"], mal_df[,"aired_to"])
mal_df <- separate(mal_df, col=premiered, sep=" ", into = c("premiered_season", "premiered_year"), remove=FALSE)

#cutoff for popularity
mal_df["isPopular"] = ifelse(mal_df[,"popularity"]<=100, TRUE, FALSE)
mal_df[,"isPopular"]

set.seed(29)
train.index <- sample(1:nrow(mal_df), nrow(mal_df)*0.65)  
train.df <- mal_df[train.index, ]
valid.df <- mal_df[-train.index, ]



# Default classification tree, optimized by pruning
#licensor studio producer duration source

default.ct <- rpart(isPopular ~ type+episodes+status+
                      airing+show_duration_months+premiered_season+premiered_year+
                      licensor+studio+producer_duration_source,
                    data = train.df, method = "class")


prp(default.ct, type=1, extra = 1)

# confusion matrix for validation
#default.ct.point.pred <- predict(default.ct, valid.df, type = "class")

# generate confusion matrix for validation data
#confusionMatrix(default.ct.point.pred, factor(valid.df$popularity))







