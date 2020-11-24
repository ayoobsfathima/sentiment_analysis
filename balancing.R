x <- read.csv(file.choose(), sep = ",")
final=x
final$polarity_level <- factor(final$polarity_level)
dresstrain=x
library(caTools)

split <- sample.split(final$polarity_level, SplitRatio = 0.75)

dresstrain <- subset(final, split == TRUE)
dresstest <- subset(final, split == FALSE)


## Let's check the count of unique value in the target variable
as.data.frame(table(dresstrain$polarity_level))

library(DMwR)

## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balanced.data <- SMOTE(polarity_level ~., dresstrain, perc.over = 4800, k = 5, perc.under = 1000)

as.data.frame(table(balanced.data$polarity_level))

df=balanced.data
----------------------------------------------------------------------------------------------------------------------------
install.packages("ROSE")
library(ROSE)

#over sampling
 data_balanced_over <- ovun.sample(polarity_level ~ ., data = hacide.train, method = "over",N = 1960)$data
 table(data_balanced_over$cls)

#under sampling
 data_balanced_under <- ovun.sample(polarity_level~ ., data = hacide.train, method = "under", N = 40, seed = 1)$data
 table(data_balanced_under$cls)