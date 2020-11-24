library(tidytext)

data=read.csv(file.choose(), stringsAsFactors = FALSE)
review_words <- data %>%
  select(id,review, star) %>%
  unnest_tokens(word,review) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

review_words
----------------------------------------------------------

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

AFINN
key=AFINN
is_key(key)
mykey <- as_key(key)
is_key(mykey)
-------------------------------------------------------
reviews_sentiment <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(id,star) %>%
  summarize(sentiment = mean(afinn_score))

reviews_sentiment
--------------------------------------------------------------------
library(ggplot2)
theme_set(theme_bw())
q=ggplot(reviews_sentiment, aes(star, sentiment, group = star)) +
  geom_boxplot() +
  ylab("Average sentiment score")
q

reviews_sentiment%>%
ggplot+geom_density(aes(sentiment))
--------------------------------------------------------------------------
#which words are more positive or negative

review_words_counted <- review_words %>%
  count(id,star, word) %>%
  ungroup()

review_words_counted

word_summaries <- review_words_counted %>%
  group_by(word) %>%
  summarize(reviews = n(),
            uses = sum(n),
            average_stars = mean(star)) %>%
  ungroup()

word_summaries





word_summaries%>%
  arrange(desc(average_stars))

word_summaries%>%
  arrange(average_stars)



ggplot(word_summaries, aes(reviews, average_stars)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  xlab("# of reviews") +
  ylab("Average Stars")

words_afinn <- word_summaries %>%
  inner_join(AFINN)

words_afinn




ggplot(words_afinn, aes(afinn_score, average_stars, group = afinn_score)) +
  geom_boxplot() +
  xlab("AFINN score of word") +
  ylab("Average stars of reviews with this word")