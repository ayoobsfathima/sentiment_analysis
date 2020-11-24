library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
# Library for parallel processing
library(doParallel)
registerDoMC(cores=detectCores())  # Use all available cores

#DATASET
x<- read.csv(file.choose(), stringsAsFactors = FALSE)
glimpse(x)
table(x$polarity_level)



#over sampling
library(ROSE)
table(x$polarity_level)
 df<- ovun.sample(polarity_level ~ ., data = x, method = "over",N =1714 )$data
 table(df$polarity_level)

#Randomize the dataset
set.seed(1)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
glimpse(df)

# Convert the 'class' variable from character to factor.
df$polarity_level <- as.factor(df$polarity_level)


#We first prepare a corpus of all the documents in the dataframe.
corpus <- Corpus(VectorSource(df$review))
# Inspect the corpus
corpus
inspect(corpus[1:3])

# Use dplyr's  %>% (pipe) utility to do this neatly.
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

dtm <- DocumentTermMatrix(corpus.clean)
# Inspect the dtm
inspect(dtm[40:50, 10:15])

#PARTITIONING THE DATA
df.train <- df[1:1285,]
df.test <- df[1286:1714,]

dtm.train <- dtm[1:1285,]
dtm.test <- dtm[1286:1714,]

corpus.clean.train <- corpus.clean[1:1285]
corpus.clean.test <- corpus.clean[1286:1714]

#FEATURE SELECTION
dim(dtm.train)

fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))
dim(dtm.train.nb)


dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))
dim(dtm.train.nb)

#NAIVE BAYES
# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

# Train the classifier
system.time( classifier <- naiveBayes(trainNB, df.train$polarity_level, laplace = 1) )


# Use the NB classifier we built to make predictions on the test set.
system.time( pred <- predict(classifier, newdata=testNB) )

# Create a truth table by tabulating the predicted class labels with the actual class labels 
table("Predictions"= pred,  "Actual" = df.test$polarity_level )

# Prepare the confusion matrix
conf.mat <- confusionMatrix(pred, df.test$polarity_level)
over=conf.mat
over