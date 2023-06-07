# Step 1 
# Install and load the required packages.
pacman::p_load(tidyr, tidytext, tidyverse, dplyr, stringr, ggplot2, textdata, yarrr, radarchart,
               wordcloud2 )

chatgpt_data <- read.csv(file.choose(), stringsAsFactors = FALSE)

#DATA CLEANING
chatgpt_data<- chatgpt_data[!chatgpt_data$author %in% 'AutoModerator',]

chatgpt <- chatgpt_data %>% select(date = utc_datetime_str, body)
glimpse(chatgpt)

# Convert to 'Date' class: 
date <- as.Date(as.character(chatgpt$date), format = "%d/%m/%Y %H:%M")

# Extract only month, month and day: 
date <- as.Date(format(date, "%Y-%m-%d"))

chatgpt$month <- format(as.Date(chatgpt$date, format = "%d/%m/%Y %H:%M"), "%B")

#changing column names
colnames(chatgpt)<-c("date","body","month")
summary(chatgpt)
chatgpt<- na.omit(chatgpt)

#customize your remove words based on the names, common words, etc.

chatgpt$body <- gsub("Äôs", "'", chatgpt$body)
chatgpt$body <- gsub("it's", "", chatgpt$body)
chatgpt$body <- gsub("preview.redd.it", "", chatgpt$body)
chatgpt$body <- gsub("x200b", "", chatgpt$body)
chatgpt$body <- gsub("can't", "", chatgpt$body)
chatgpt$body <- gsub("_is_chatgpt", "", chatgpt$body)
chatgpt$body <- gsub("amp x200b", "", chatgpt_data$body)
chatgpt$body <- gsub("png amp", "enabled amp", chatgpt$body)
chatgpt$body<- gsub("setup file", "", chatgpt$body)
chatgpt$body<- gsub("amp auto", "", chatgpt$body)
chatgpt$body <- gsub("NA NA", "", chatgpt$body)
chatgpt$body <- gsub("pjpg", "", chatgpt$body)


remove_words <- c("ChatGPT","gpt","dan","png","amp","0","1","2","3","4","5","6","7","8","9", "it's", "In", "order", "to", "prevent", "multiple", "repetitive", "comments", "this", "is", "a", 
                  "friendly", "request", "to", "reply", "to", "this", "comment","with", "the", "prompt", "they", "used", "so", "other",
                  "users", "can", "experiment", "with", "it", "as", "well", "&","gt","https","www","gptoverflow","link", "Google", 
                  "preview","redd.it", "width", "=803&amp","format", "png&amp", "google", "chatgpt","preview", "yeah","webp","
it's","auto=webp&amp","x200b:", "www.reddit.com","preview.reddit.it:", "NA NA","amp auto", "setup file","png amp","enabled amp",
                  "amp x200b", "_is_chatgpt")


# Step 3 – Delete these words 
#unnest and remove stop, undesirable and short words

chatgpt %>% select(body) %>% unnest_tokens(word,body) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

chatgpt_words_filtered <- chatgpt %>%
  unnest_tokens(word, body) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

# Step 4 – get the full word count (filtered)
chatgpt_word_summary <- chatgpt_words_filtered %>%
  group_by(month = format(as.Date(date, "%d/%m/%Y"), "%B")) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(month, word_count) %>%
  distinct() %>% 
  ungroup()

# Step 5 – plot the most commonly used words 
chatgpt_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  xlab("") +
  ylab("Word Count") +
  ggtitle("Most Frequently Used Words in Reddit r/ChatGPT Comments") +
  coord_flip()

chatgpt_words_filtered %>%
  filter(date >= '01/03/2023' & date <= '31/03/2023') %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  xlab("") +
  ylab("Word Count") +
  ggtitle("Most Frequently Used Words in Reddit r/ChatGPT Comments in March") +
  coord_flip()

# Step 6 – create a cool wordcloud of the words
chatgpt_word_counts <- chatgpt_words_filtered %>% 
  count(word, sort = TRUE)
wordcloud2(chatgpt_word_counts[1:300, ], size = .5)

# Step 6a – create a cool wordcloud of the words
chatgpt_word_counts <- chatgpt_words_filtered %>% 
  filter(date >= '01/03/2023' & date <= '31/03/2023') %>%
  count(word, sort = TRUE)
wordcloud2(chatgpt_word_counts[1:300, ], size = .5)

# Step 6b – create a cool wordcloud of the words
chatgpt_word_counts <- chatgpt_words_filtered %>% 
  filter(date >= '01/02/2023' & date <= '28/02/2023') %>%
  count(word, sort = TRUE)
wordcloud2(chatgpt_word_counts[1:300, ], size = .5)


# Step 6c – create a cool wordcloud of the words
chatgpt_word_counts <- chatgpt_words_filtered %>% 
  filter(date >= '01/01/2023' & date <= '31/01/2023') %>%
  count(word, sort = TRUE)
wordcloud2(chatgpt_word_counts[1:300, ], size = .5)

# Step 6d – create a cool wordcloud of the words
chatgpt_word_counts <- chatgpt_words_filtered %>% 
  filter(date >= '01/12/2022' & date <= '31/12/2022') %>%
  count(word, sort = TRUE)
wordcloud2(chatgpt_word_counts[1:300, ], size = .5)


# Step 7 – Lets see if the comments' lexical diversity has changed  over the months

# first we make a tidy text version of the data we have
chatgpt_tidy <- chatgpt_data %>%
  unnest_tokens(word, body) %>%
  filter(!word %in% remove_words) %>%
  filter(!nchar(word) < 3) %>%
  anti_join(stop_words)

# Convert to 'Date' class: 
date <- as.Date(as.character(chatgpt_tidy$utc_datetime_str), format = "%d/%m/%Y %H:%M")
chatgpt_tidy$date <- as.Date(format(date, "%Y-%m-%d"))



# calculate the number of words per month 
chatgpt_lexical_diversity <- chatgpt_tidy %>%
  mutate(month = lubridate::month(date)) %>%
  group_by(month, word) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(word, month, word_count) %>%
  distinct() %>% 
  ungroup()

# Step 8 - display a pirate plot of the distribution of words over the months
pirateplot(formula = word_count ~ month, #Formula
           data = chatgpt_lexical_diversity, #Data frame we just created
           xlab = NULL, ylab = " Distinct Word Count by Month", #Axis labels
           main = "Lexical Diversity Per Month", #Plot title
           pal = "google", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme Change these from 1 to 5 to see different display themes
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size



#step 9 - 
nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)


chatgpt_tidy$month <- format(as.Date(chatgpt_tidy$date, format = "%d/%m/%Y %H:%M"), "%B")

chatgpt_nrc <- chatgpt_tidy %>% inner_join(get_sentiments("nrc"))

ggplot(chatgpt_nrc, aes(x = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments in December 2022")

chatgpt_words_filtered %>%
  filter(date >= '01/12/2022' & date <= '31/3/2023') %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Overall Sentiments on ChatGPT")


#Step 10 - nrc by months

chatgpt_words_filtered %>%
  filter(date >= '01/12/2022' & date <= '31/12/2022') %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments in December 2022")

chatgpt_words_filtered %>%
  filter(date >= '01/01/2023' & date <= '31/01/2023') %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments in January 2023")

chatgpt_words_filtered %>%
  filter(date >= '01/02/2023' & date <= '28/02/2023') %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments in February 2023")

chatgpt_words_filtered %>%
  filter(date >= '01/03/2023' & date <= '31/03/2023') %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments in March 2023")

#Step 11
#first get the monthly sentiment for all the data we have in chatgpt_nrc
chatgpt_monthly_sentiment_nrc <- chatgpt_nrc %>%
  group_by(month, sentiment) %>%
  count(month, sentiment) %>%
  select(month, sentiment, sentiment_month_count = n)

ggplot(chatgpt_monthly_sentiment_nrc, aes(x = month, y = sentiment_month_count, fill =
                                            sentiment)) +
  geom_col(position = "dodge2") +
  ggtitle("Sentiments By Months")

#RADARCHART


# get the count of sentiment words per month
chatgpt_total_sentiment_words_permonth <- chatgpt_nrc %>%
  count(month) %>%
  select(month, month_total = n)

#put these two together to create a percentage figure for month over month
chatgpt_month_over_month_sentiment <- chatgpt_monthly_sentiment_nrc %>%
  inner_join(chatgpt_total_sentiment_words_permonth, by = "month") %>%
  mutate(percent = sentiment_month_count / month_total * 100 ) %>%
  select(-sentiment_month_count, -month_total) %>%
  spread(month, percent)

chatgpt_total_sentiment_words_permonth <- chatgpt_total_sentiment_words_permonth %>%
  select('sentiment','December','January','February','March','April')

#radarchart
chartJSRadar(chatgpt_month_over_month_sentiment)

#COMPARING DEC AND MAR

chatgpt_December_vs_March_sentiment <- chatgpt_monthly_sentiment_nrc %>%
  inner_join(chatgpt_total_sentiment_words_permonth, by = "month") %>%
  mutate(percent = sentiment_month_count / month_total * 100 ) %>%
  filter(month %in% c("December","March")) %>%
  select(-sentiment_month_count, -month_total) %>%
  spread(month, percent)
# plot it out with a radar chart
chartJSRadar(chatgpt_December_vs_March_sentiment)

#BIGRAMS
# first extract all bigrams
chatgpt_bigrams <- chatgpt %>%
  unnest_tokens(bigram, body, token = "ngrams", n = 2)
# second separate and filter the bigrams 
chatgpt_bigrams_separated <- chatgpt_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")


chatgpt_bigrams_filtered <- chatgpt_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% remove_words) %>%
  filter(!word2 %in% remove_words)


bigrams_separated <chatgpt %>% select(body) %>% 
  unnest_tokens(ngrams,body,token="ngrams",n=2) %>%
  separate(ngrams, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)




