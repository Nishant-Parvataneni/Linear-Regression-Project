df<- read.csv('student-mat.csv' , sep=';')
# https://archive.ics.uci.edu/ml/datasets/Student+Performance

head(df)
summary(df)

library(ggplot2)
library(ggthemes)
library(dplyr)

num.cols<- sapply(df, is.numeric)

# install.packages('corrgram',repos = 'http://cran.us.r-project.org')
# install.packages('corrplot',repos = 'http://cran.us.r-project.org')

cor.data<- cor(df[ ,num.cols])
cor.data

library(corrplot)
corrplot(cor.data, method='color')

ggplot(df,aes(x=G3)) + geom_histogram(bins=20, alpha=0.5, fill='green')

library(caTools)

sample<- sample.split(df$age, SplitRatio = 0.70)
train = subset(df, sample==TRUE)
test = subset(df, sample==FALSE)

model<- lm(G3~ .,train)
summary(model)

res<- residuals(model)
res<- as.data.frame(res)

head(res)

ggplot(res,aes(res)) + geom_histogram(bins=20,alpha=0.5, fill='blue')
