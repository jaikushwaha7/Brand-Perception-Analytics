api_key = 'OUwGpLqhQFEPG6MrAoNcbWqMi'
api_secret_key = 'LjDhwhcdAgeHiZcGsBMsKc7mLwTsIZEt8FHvjl6eXFD5YiaTE5'
access_token = '795881726-N2cWgPyM8Ppux1NceZK7BmgrBeWhvPssfXcO1N8y'
access_token_secret = 'yoGkcdjraIqsBCd4qOKaTsWrYlsGUJ2icKdkPXmDS8O7w'


## authenticate via web browser
token <- create_token(
  app = "Web and Social Analytics",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

library(rtweet)
get_token()

setup_twitter_oauth(api_key, api_secret_key, access_token, access_token_secret)

save_as_csv(tweet_az_eng,file_name = "amazon_tweets.csv",prepend_ids = T,na ="", fileEncoding = "UTF-8")

terms1 = c("amazon", "#Amazon", "Amazon")
terms2 = c("flipkart", "#flipkart", "Flipkart")

terms_search1 = paste(terms1, collapse = " OR ")
terms_search2 = paste(terms2, collapse = " OR ")

tweet_amazon = searchTwitter(terms_search1, n = 2000, lang ="en")
tweet_flipkart = searchTwitter(terms_search2, n=2000, lang = "en")

tweet_amazon = twListToDF(tweet_amazon)
tweet_flipkart = twListToDF(tweet_flipkart)

write.table(tweet_amazon, "D:/Study/Great Lakes/Web and Social analytics/grp assignment/tweet_amazon.csv", 
            append =T, row.names= F, col.names = T, sep = "," )
write.table(tweet_flipkart, "D:/Study/Great Lakes/Web and Social analytics/grp assignment/tweet_flipkart.csv", 
            append =T, row.names= F, col.names = T, sep = "," )

getwd()

summary(tweet_amazon)
summary(tweet_flipkart)

# Remove retweets
tweet_amazon_nrtwts <- tweet_amazon[tweet_amazon$isRetweet==FALSE, ] 
# Remove replies
tweet_flipkart_nrtwts <- tweet_flipkart[tweet_flipkart$isRetweet==FALSE, ] 
nrow(tweet_amazon_nrtwts)
nrow(tweet_flipkart_nrtwts)

# Remove replies
tweet_amazon_nrtwts <- subset(tweet_amazon_nrtwts, is.na(tweet_amazon_nrtwts$replyToSID)) 
tweet_flipkart_nrtwts <- subset(tweet_flipkart_nrtwts, is.na(tweet_flipkart_nrtwts$replyToSID)) 

nrow(tweet_amazon_nrtwts)
nrow(tweet_flipkart_nrtwts)

# Analyse engagement by looking at the variables: favoriteCount (i.e. the number of likes)
# or retweetCount (i.e. the number of retweets). Simply arrange them in descending order (with a minus "-" before the variable) 
# to find the one with the highest number of likes or retweets or ascending order (without the minus) to find the one with lowest 
# number of engagements.

# MOst favourite
tweet_amazon_nrtwts <- tweet_amazon_nrtwts %>% arrange(-favoriteCount)
tweet_amazon_nrtwts[1,5]

tweet_flipkart_nrtwts <- tweet_flipkart_nrtwts %>% arrange(-favoriteCount)
tweet_flipkart_nrtwts[1,5]

# Highest retweets
tweet_amazon_nrtwts <- tweet_amazon_nrtwts %>% arrange(-retweetCount)
tweet_amazon_nrtwts[1,5]

tweet_flipkart_nrtwts <- tweet_flipkart_nrtwts %>% arrange(-retweetCount)
tweet_flipkart_nrtwts[1,5]

# Least favourite
tweet_amazon_nrtwts <- tweet_amazon_nrtwts %>% arrange(favoriteCount)
tweet_amazon_nrtwts[1,5]

tweet_flipkart_nrtwts <- tweet_flipkart_nrtwts %>% arrange(favoriteCount)
tweet_flipkart_nrtwts[1,5]

# Lowest retweets
tweet_amazon_nrtwts <- tweet_amazon_nrtwts %>% arrange(retweetCount)
tweet_amazon_nrtwts[1,5]

tweet_flipkart_nrtwts <- tweet_flipkart_nrtwts %>% arrange(retweetCount)
tweet_flipkart_nrtwts[1,5]

# RATIO OF REPLIES/RETWEETS/ORGANIC (FRESH TWEETS)

# Keeping only the retweets
amazon_retweet <- tweet_amazon[tweet_amazon$isRetweet==TRUE,]
flipkart_retweet <- tweet_flipkart[tweet_flipkart$isRetweet==TRUE,]
# Keeping only the replies
amazon_replies <- subset(tweet_amazon, !is.na(tweet_amazon$replyToSID))
flipkart_replies <- subset(tweet_flipkart, !is.na(tweet_flipkart$replyToSID))

# Creating a data frame
data_amazon <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(nrow(tweet_amazon_nrtwts), nrow(amazon_retweet), nrow(amazon_replies))
)

data_flipkart <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(nrow(tweet_flipkart_nrtwts), nrow(flipkart_retweet), nrow(flipkart_replies))
)

# Calculations and metrics for amazon tweets
data_amazon$fraction = data_amazon$count / sum(data_amazon$count)
data_amazon$percentage = data_amazon$count / sum(data_amazon$count) * 100
data_amazon$ymax = cumsum(data_amazon$fraction)
data_amazon$ymin = c(0, head(data_amazon$ymax, n=-1))# Rounding the data_amazon to two decimal points
data_amazon <- round_df(data_amazon, 2)# Specify what the legend should say
Type_of_Tweet <- paste(data_amazon$category, data_amazon$percentage, "%") 
ggplot(data_amazon, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


# Calculations and metrics for flipkart tweets
data_flipkart$fraction = data_flipkart$count / sum(data_flipkart$count)
data_flipkart$percentage = data_flipkart$count / sum(data_flipkart$count) * 100
data_flipkart$ymax = cumsum(data_flipkart$fraction)
data_flipkart$ymin = c(0, head(data_flipkart$ymax, n=-1))# Rounding the data_flipkart to two decimal points
data_flipkart <- round_df(data_flipkart, 2)# Specify what the legend should say
Type_of_Tweet <- paste(data_flipkart$category, data_flipkart$percentage, "%")
ggplot(data_flipkart, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# TS plot of amazon tweets
colnames(tweet_amazon)[colnames(tweet_amazon)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(tweet_amazon, tweet_amazon$Twitter_Account), "year") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from Amazon",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )



# ## stream tweets mentioning amazon and flipkart for 10 mins
# rt <- rtweet::stream_tweets( q = "amazon,flipkart",
#    timeout = (60 * 60 * 10))
 
# ts.df <- ts_plot(rt, by = "mins", filter = c(
#     "amazon", "flipkart", thirdpop),
#     main = "amazon vs flipkart")

# From where the tweets are published for amazon
amazon_app <- tweet_amazon %>% 
  select(statusSource) %>% 
  group_by(statusSource) %>%
  summarize(count(statusSource))
summary(amazon_app)  ## mean source is instagram
plot(amazon_app)
amazon_app <- subset(amazon_app, statusSource.x> 11)
barplot(tweet_amazon$statusSource)
summary(tweet_amazon$statusSource)


# Clean tweets symbols and punctuations
tweet_amazon_nrtwts$text <-  gsub("https\\s*", "", tweet_amazon_nrtwts$text)
tweet_amazon_nrtwts$text <-  gsub("@\\s*", "", tweet_amazon_nrtwts$text) 
tweet_amazon_nrtwts$text  <-  gsub("amp", "", tweet_amazon_nrtwts$text) 
tweet_amazon_nrtwts$text  <-  gsub("[\r\n]", "", tweet_amazon_nrtwts$text)
tweet_amazon_nrtwts$text  <-  gsub("[[:punct:]]", "", tweet_amazon_nrtwts$text)

tweets <- tweet_amazon_nrtwts %>%  select(text) %>%  unnest_tokens(word, text)
tweets <- tweets %>%  anti_join(stop_words)

#### YES BAnk

tweet.df = read.csv("yes_bank_2tweets.csv", header = T)
# Remove retweets
yes_bank_2tweets_unique <- tweet.df[tweet.df$isRetweet==FALSE, ] 
nrow(yes_bank_2tweets_unique)


# Remove replies
yes_bank_2tweets_unique <- subset(yes_bank_2tweets_unique, is.na(yes_bank_2tweets_unique$replyToSID)) 
nrow(yes_bank_2tweets_unique)


# Analyse engagement by looking at the variables: favoriteCount (i.e. the number of likes)
# or retweetCount (i.e. the number of retweets). Simply arrange them in descending order (with a minus "-" before the variable) 
# to find the one with the highest number of likes or retweets or ascending order (without the minus) to find the one with lowest 
# number of engagements.

# MOst favourite
yes_bank_2tweets_unique <- yes_bank_2tweets_unique %>% arrange(-favoriteCount)
yes_bank_2tweets_unique[1,5]


# Highest retweets
yes_bank_2tweets_unique <- yes_bank_2tweets_unique %>% arrange(-retweetCount)
yes_bank_2tweets_unique[1,5]

# Least favourite
yes_bank_2tweets_unique <- yes_bank_2tweets_unique %>% arrange(favoriteCount)
yes_bank_2tweets_unique[1,5]


# Arranging by desecnding no. of retweets and least favourite tweet
yes_bank_2tweets_unique <- yes_bank_2tweets_unique %>% arrange(retweetCount)
yes_bank_2tweets_unique[1,5]


# RATIO OF REPLIES/RETWEETS/ORGANIC (FRESH TWEETS)

# Keeping only the retweets
yes_bank_retweet <- tweet.df[tweet.df$isRetweet==TRUE,]

# Keeping only the replies
yes_bank_replies <- subset(tweet.df, !is.na(tweet.df$replyToSID))

# Creating a data frame
data_yes_bank <- data.frame(
  category=c("Unique", "Retweets", "Replies"),
  count=c(nrow(yes_bank_2tweets_unique), nrow(yes_bank_retweet), nrow(yes_bank_replies))
)


# Calculations and metrics for amazon tweets
data_yes_bank$fraction = data_yes_bank$count / sum(data_yes_bank$count)
data_yes_bank$percentage = data_yes_bank$count / sum(data_yes_bank$count) * 100
data_yes_bank$ymax = cumsum(data_yes_bank$fraction)
data_yes_bank$ymin = c(0, head(data_yes_bank$ymax, n=-1))# Rounding the data_yes_bank to two decimal points
data_yes_bank <- round_df(data_yes_bank, 2)# Specify what the legend should say
Type_of_Tweet <- paste(data_yes_bank$category, data_yes_bank$percentage, "%") 
ggplot(data_yes_bank, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

colnames(tweet.df)[colnames(tweet.df)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(tweet.df, tweet.df$created), "year") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets related to Yes Bank",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#rt <- rtweet::stream_tweets( q = "yesbank",    timeout = (60 * 60 * 10))
 
ts.df <- ts_plot(rt, by = "mins", filter = c("yesbank", "corona"), main = "yes bank vs corona")

# From where the tweets are published for amazon
yes_bank_app <- tweet.df %>% 
  select(statusSource) %>% 
  group_by(statusSource) %>%
  summarize(count =count(statusSource)) 
data_table_yes = data.table(yes_bank_app)
data_table_yes
data_table_yes = data_table_yes %>%
summary(yes_bank_app)
## most are from instagram as median 
## On on average basis India Today is posting a tweet.

plot(yes_bank_app)
boxplot(yes_bank_app$`count(statusSource)`)

data <- data.frame(
  category=yes_bank_app$source,
  count=yes_bank_app$`count(statusSource)`
)
data
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
data <- round_df(data, 2)
Source <- paste(data$category, data$percentage, "%")

Source
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


#First we need to count how many replies a user gets...
r_table <- table(tweet.df$replyToSN)
#..rank them...
r_levels <- names(r_table)[order(-r_table)]
#..and use this ordering to order the factor levels...
tweet.df$replyToSN <- factor(tweet.df$replyToSN, levels = r_levels) 

#Then we can plot the chart...
ggplot(subset(tweet.df,subset=(!is.na(replyToSN))),aes(x=replyToSN)) + geom_bar(aes(y = (..count..)))+     options(axis.text.x=theme_text(angle=-90,size=6))

#label a tweet with the month number
tw.dfs$month=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$mon})
#label a tweet with the hour
tw.dfs$hour=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$hour})
#label a tweet with a number corresponding to the day of the week
tw.dfs$wday=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$wday})

#We can also generate barplots showing the distribution of tweet count over time:
ggplot(tw.dfs,aes(x=created))+geom_bar(aes(y = (..count..)))