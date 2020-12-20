library(rpart)
library(rpart.plot)
library(caret)
library(tidyr)
library(pROC)
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
mal_df["isPopular"] = ifelse(mal_df[,"popularity"]<=200, TRUE, FALSE)
mal_df[,"isPopular"]


#print(mal_df[,"genre"])
mal_df[,"genre"] <- as.character(mal_df[,"genre"])
all_genres <- c("filler")
for (row in 1:nrow(mal_df)){
  split_genres <- strsplit(mal_df[row,"genre"], ",")
  for(single_genre in split_genres){
    #remove white space
    single_genre <- gsub(" ", "", single_genre, fixed = TRUE)
    #single_genre <- trimws(, which="both")
      all_genres <- c(all_genres, single_genre)
  }
}

all_genres <- all_genres[1:length(all_genres)]
all_genres <- unique(all_genres)
all_genres[21] <- "SciFi"
print(all_genres)

mal_df[all_genres] <- 0

mal_df[,"genre"] <- as.character(mal_df[,"genre"])

head(mal_df)

for(row in 1:nrow(mal_df)){
  row_genres <- mal_df[row,"genre"]
  split_genres <- strsplit(row_genres, ",")
  for(single_genre in split_genres){
    #remove white space
    single_genre <- gsub(" ", "", single_genre, fixed = TRUE)
    mal_df[row,single_genre] <- 1

  }
}
#print(head(mal_df))


set.seed(29)
train.index <- sample(1:nrow(mal_df), nrow(mal_df)*0.65)  
train.df <- mal_df[train.index, ]
valid.df <- mal_df[-train.index, ]

names(train.df)

# Default classification tree, optimized by pruning
#licensor studio producer duration source
print(names(train.df)[39:81])
paste(make.names(names(train.df)[39:81]), collapse="+")


default.ct <- rpart(isPopular ~ type+episodes+status+airing+show_duration_months+
                      premiered_season+premiered_year+
                      licensor+studio+producer+duration+source+
                      Comedy+Supernatural+Romance+Shounen+Parody+School+Magic+Shoujo+Drama+Fantasy+Kids+
                      Action+Music+SliceofLife+Josei+Harem+ShounenAi+Adventure+SuperPower+SciFi+Ecchi+
                      Seinen+MartialArts+Game+Sports+Demons+Historical+Horror+Mystery+Samurai+Hentai+Space+
                      Mecha+Psychological+Police+Vampire+Military+Thriller+Yaoi+Cars+ShoujoAi+Dementia+Yuri,
                    data = train.df, method = "class")

rpart.rules(default.ct, style = "tallw")
#prp(default.ct, type=1, extra = 1)

# confusion matrix for validation
#default.ct.point.pred <- predict(default.ct, valid.df, type = "class")

# generate confusion matrix for validation data
#confusionMatrix(default.ct.point.pred, factor(valid.df$popularity))


print("Logistic Regression")
logit.reg <- glm(isPopular~type+episodes+status+airing+show_duration_months+
                   premiered_season+
                   licensor+studio+producer+duration+source+
                   Comedy+Supernatural+Romance+Shounen+Parody+School+Magic+Shoujo+Drama+Fantasy+Kids+
                   Action+Music+SliceofLife+Josei+Harem+ShounenAi+Adventure+SuperPower+SciFi+Ecchi+
                   Seinen+MartialArts+Game+Sports+Demons+Historical+Horror+Mystery+Samurai+Hentai+Space+
                   Mecha+Psychological+Police+Vampire+Military+Thriller+Yaoi+Cars+ShoujoAi+Dementia+Yuri,
                 data = train.df, family = "binomial")
print("model is done")
summary(logit.reg)
