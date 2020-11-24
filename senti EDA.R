library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)



review=read.csv(file.choose(), stringsAsFactors = FALSE)
names(review)
## Make a vector source and a corpus
corpus_review=Corpus(VectorSource(review$review))
#data preprocessing
corpus_review=tm_map(corpus_review, tolower)
corpus_review=tm_map(corpus_review, removePunctuation)
#Remove stopwords
corpus_review=tm_map(corpus_review, removeWords, stopwords("english"))
# Remove context specific stop words
corpus_review=tm_map(corpus_review, removeWords,c("pan","also", "get","like", "company", "made", "can", "im", "abdul", "just", "i"))
## Stem document
corpus_review=tm_map(corpus_review, stemDocument)
##Viewing the corpus content
corpus_review[[8]][1]
# Find the 10 most frequent terms: term_count
term_count <- freq_terms(corpus_review)
# Plot 10 most frequent terms
df1=data.frame(word=term_count$WORD, freq=term_count$FREQ)
# Plot 10 most frequent terms
best=ggplot(data=df1[1:10,], aes(x=word, y=freq)) +
  geom_bar(stat="identity", width=0.5,fill="#3399CC")+
labs(title = "MOST FREQUENTLY USED WORDS")+
 theme(plot.title = element_text(color = "#000000", size = 12, face = "bold",hjust=0.5))
best

review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)
# Convert TDM to matrix
review_m <- as.matrix(review_tdm)
# Sum rows and frequency data frame
review_term_freq <- rowSums(review_m)
# Sort term_frequency in descending order
review_term_freq <- sort(review_term_freq, decreasing = T)
# Create a wordcloud for the values in word_freqs
wordcloud(review_word_freq$term, review_word_freq$num,max.words = 50, colors = "red")
# Print the word cloud with the specified colors
wordcloud(review_word_freq$term, review_word_freq$num,max.words = 50, colors = c("aquamarine","darkgoldenrod","tomato"))

fat=data.frame(review_word_freq$term,review_word_freq$num)
library(wordcloud2)
# Gives a proposed palette
wordcloud2(fat, size=1.6, color='random-dark')
wordcloud2(fat, size=1.6, color='random-light', backgroundColor="black")
wordcloud2(fat, size = 0.7, shape = 'star')
wordcloud2(fat,figPath= "twitter.jpg")
wordcloud2(demoFreq, figPath = "twitter.jpg")


#CORPUS BAD REVIEW
review_yes=subset(review$review,review$star==2 | review$star==1)
corpus_review_yes=Corpus(VectorSource(review_yes))
#data preprocessing
corpus_review_yes=tm_map(corpus_review_yes, tolower)
corpus_review_yes=tm_map(corpus_review_yes, removePunctuation)
#Remove stopwords
corpus_review_yes=tm_map(corpus_review_yes, removeWords, stopwords("english"))
# Remove context specific stop words
corpus_review_yes=tm_map(corpus_review_yes, removeWords,c("also", "get","like", "food", "made", "can", "im", "asian", "i"))
## Stem document
corpus_review_yes=tm_map(corpus_review_yes, stemDocument)
##Viewing the corpus content
corpus_review_yes[[8]][1]
# Find the 20 most frequent terms: term_count
term_count_n <- freq_terms(corpus_review_yes, 20)
df=data.frame(word=term_count_n$WORD, freq=term_count_n$FREQ)
# Plot 20 most frequent terms

worst=ggplot(data=df[1:10,], aes(x=word, y=freq)) +
  geom_bar(stat="identity", width=0.5,fill="red")+
labs(title = "MOST FREQUENTLY USED WORDS IN POOR RATINGS")+
 theme(plot.title = element_text(color = "#000000", size = 12, face = "bold",hjust=0.5))
worst

data=data.frame(rating=c(1,2,3,4,5),freq=c(6,6,34,98,734))
data
stars=ggplot(data=data, aes(x=rating, y=freq)) +
  geom_bar(stat="identity", width=0.5,fill="#FFFF66")+
labs(title = "RATINGS")+
 theme(plot.title = element_text(color = "#000000", size = 12, face = "bold",hjust=0.5))
stars

