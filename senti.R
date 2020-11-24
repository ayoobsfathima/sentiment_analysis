library(plyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(dplyr)
install.packages("sentimentr")
library(sentimentr)
library(tidytext)
install.packages("wesanderson")
# Load
library(wesanderson)


data=read.csv(file.choose(), stringsAsFactors = FALSE)
head(data)
colnames(data)

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

AFINN
key=AFINN
is_key(key)
mykey <- as_key(key)
is_key(mykey)

data1=sentiment(data$review, polarity_dt=mykey)
max(data1$sentiment)
data2=data1
data3=data2 %>% 
  rename(id=element_id)

data3=merge(data3,data,by="id")
data4=data3[!duplicated(data3$id)]
View(data4)

data4%>%
ggplot+geom_density(aes(sentiment))




theme_set(theme_bw())
p=ggplot(data4, aes(star, sentiment, group = star)) +
  geom_boxplot() +
  ylab("Average sentiment score")
p



data4=data4%>% 
  mutate(polarity_level = ifelse(sentiment< 0, "Negative", "Positive"))
View(data4)


------------------------------------------------------------------------------------------------------
data5=read.csv(file.choose(), stringsAsFactors = FALSE)
head(data5)
colnames(data5)



data6=sentiment(data5$review)

data6%>%
ggplot+geom_density(aes(sentiment))

data7=data5 %>%
      get_sentences() %>%
      sentiment () %>% 
  mutate(polarity_level = ifelse(sentiment< 0, "Negative", "Positive"))


data7=data7[!duplicated(data7$id),]
View(data7)

theme_set(theme_bw())
q=ggplot(data7, aes(star, sentiment, group = factor(star))) +
  geom_boxplot() +
  ylab("Average sentiment score")+
  scale_fill_manual(values=wes_palette(n=5, name="Rushmore1"))
q

----------------------------------------------------------------------------------------------------------------------
write.csv(data7,"C://Users//Sheeja Ayoob//Desktop//SENTI//sentiment_final2.csv")


library(gridExtra)
plot1 <- p
plot2 <- q
grid.arrange(plot1, plot2, ncol=2)
--------------------------------------------------------------------------------------------
