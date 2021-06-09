#install.packages("textdata")

library(ggplot2)
library(scales)
library(tidytext)
library(textdata)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(lubridate)
#Sentiment lexicons
get_sentiments(lexicon = 'bing')
unique(get_sentiments("bing")$sentiment)

#load data
setwd("/Users/hyoungminlee/Desktop/SPRING/NLP/project")
data <- read.csv("tweets_revised.csv", na.strings = c("","NA"))

data.words <- data %>%
  select(-created_at) %>%
  unnest_tokens(word, clean_tweet)

#Use bing to get sentiment
data.words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE)

#Extract top 10 positive/negative sentiment using bing
data.sentiment <- data.words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment)%>%
  top_n(10,n) %>%
  ungroup() %>%
  mutate(nsign = ifelse(sentiment == "negative", -n, n))

# Graph the frequently appeared words sentiment after removing words that convey incorrect sentiment
data.sentiment2 <- data.words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(!(word =="like"|word =="positive"|word =="negative"|word =="free"|word =="pan"|word =="well"|
             word =="virus"|word =="infection"|word =="outbreak")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment)%>%
  top_n(10,n) %>%
  ungroup() %>%
  mutate(nsign = ifelse(sentiment == "negative", -n, n))

ggplot(data.sentiment2,
       aes(x=reorder(word, n), y=n,
           fill = factor(sentiment,
                         levels = c("positive", "negative")))) +
  geom_col(color ="lightslategray", width=0.8, show.legend=FALSE) + 
  scale_fill_manual(values = c("skyblue","pink")) +
  geom_text(aes(label = n), size = 3, color = "black", hjust = 1.1) +
  facet_wrap(~ factor(sentiment,
                      levels = c("positive", "negative")), ncol = 2, scales ="free") +
  labs(x="Words", y="Count") +
  coord_flip()

#Change the date format
data.words$date <- format(as.Date(data.words$date), "%Y-%m-%d")
data.words$date <- strptime(data.words$date, format= "%Y-%m-%d")

#Create new_df
new_df <- data.words
new_df$week <- strftime(new_df$date, format = "%V")
sum(is.na(new_df$week))
new_df <-new_df[!is.na(new_df$date), ]
sum(is.na(data.words$date))

#Time series for sentiment
new_df2 <- new_df %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(!(word =="infection"|word =="virus"|word =="outbreak"|word =="pan"|
             word =="like"|word =="top"|word =="well"|word =="positive"|word =="negative"|word =="shot"|
             word =="patient"|word =="issue"|word =="rise"|word =="case"|word =="vaccine"|word =="pandemic"|
             word =="young"|word =="old"|word =="ministry"|word =="government"|word =="fire"|word =="trip"|
             word =="trump"|word =="right"|word =="infected"|word =="symptom")) %>% 
  count(sentiment, date) %>%
  group_by(sentiment) %>%
  slice(2:(n()-1)) %>%
  ungroup()

new_df2$date <- as.POSIXct(new_df2$date)
Sys.setlocale("LC_TIME", "English")
ggplot(new_df2, aes(x=date, y=n, fill=sentiment, color=sentiment))+
  geom_area(position="identity", alpha = 0.25)+
  geom_line(size = 1.3) +
  scale_fill_manual(labels=c("Negative","Positive"),
                    values = c("pink","skyblue")) +
  scale_color_manual(labels=c("Negative","Positive"),
                     values = c("pink","skyblue")) +
  scale_x_datetime(date_labels = "%b %d", date_break = "1 week")+
  labs(x='Date',y='count')+
  theme(legend.title = element_blank())


############################"TWEETS MENTIONING MODI"############################

modi <- read.csv("tweets_modi_rev.csv", na.strings = c("","NA"))

modi.words <- modi %>%
  select(-created_at) %>%
  unnest_tokens(word, clean_tweet)

#Use bing to get sentiment
modi.words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE)

#Extract top 10 positive/negative sentiment using bing
modi.sentiment <- modi.words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment)%>%
  top_n(10,n) %>%
  ungroup() %>%
  mutate(nsign = ifelse(sentiment == "negative", -n, n))

# Graph the frequently appeared words sentiment after removing words that convey incorrect sentiment
modi.sentiment2 <- modi.words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(!(word =="like"|word =="positive"|word =="negative"|word =="free"|word =="pan"|word =="well"|
             word =="virus"|word =="infection"|word =="outbreak")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment)%>%
  top_n(10,n) %>%
  ungroup() %>%
  mutate(nsign = ifelse(sentiment == "negative", -n, n))

ggplot(modi.sentiment2,
       aes(x=reorder(word, n), y=n,
           fill = factor(sentiment,
                         levels = c("positive", "negative")))) +
  geom_col(color ="lightslategray", width=0.8, show.legend=FALSE) + 
  scale_fill_manual(values = c("skyblue","pink")) +
  geom_text(aes(label = n), size = 3, color = "black", hjust = 1.1) +
  facet_wrap(~ factor(sentiment,
                      levels = c("positive", "negative")), ncol = 2, scales ="free") +
  labs(x="Words", y="Count") +
  coord_flip()

#Change the date format
modi.words$date <- format(as.Date(modi.words$date), "%Y-%m-%d")
modi.words$date <- strptime(modi.words$date, format= "%Y-%m-%d")

#Create new_df_modi
new_df_modi <- modi.words
new_df_modi$week <- strftime(new_df_modi$date, format = "%V")
sum(is.na(new_df_modi$week))
new_df_modi <-new_df_modi[!is.na(new_df_modi$date), ]
sum(is.na(modi.words$date))

#Time series for sentiment
new_df2_modi <- new_df_modi %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(!(word =="infection"|word =="virus"|word =="outbreak"|word =="pan"|
             word =="like"|word =="top"|word =="well"|word =="positive"|word =="negative"|word =="shot"|
             word =="patient"|word =="issue"|word =="rise"|word =="case"|word =="vaccine"|word =="pandemic"|
             word =="young"|word =="old"|word =="ministry"|word =="government"|word =="fire"|word =="trip"|
             word =="trump"|word =="right"|word =="infected"|word =="symptom")) %>% 
  count(sentiment, date) %>%
  group_by(sentiment) %>%
  slice(2:(n()-1)) %>%
  ungroup()

new_df2_modi$date <- as.POSIXct(new_df2_modi$date)
Sys.setlocale("LC_TIME", "English")
ggplot(new_df2_modi, aes(x=date, y=n, fill=sentiment, color=sentiment))+
  geom_area(position="identity", alpha = 0.25)+
  geom_line(size = 1.3) +
  scale_fill_manual(labels=c("Negative","Positive"),
                    values = c("pink","skyblue")) +
  scale_color_manual(labels=c("Negative","Positive"),
                     values = c("pink","skyblue")) +
  scale_x_datetime(date_labels = "%b %d", date_break = "1 week")+
  labs(x='Date',y='count')+
  theme(legend.title = element_blank())





