library(dplyr)
library(ggplot2)
library(matrixStats)
library(tm)
library(knitr)

descriptions <- read.csv('/Users/nataliecarlson/Desktop/Tech Hubs/descriptions.csv',
                         header=TRUE, stringsAsFactors=FALSE)
descriptions <- descriptions[,-1]

master <- read.csv('/Users/nataliecarlson/Downloads/SSA_Startups 3.csv',
                   header=TRUE, stringsAsFactors=FALSE)

master <- master[-1,13:884]
master[master==2] <- 0
good_data <- filter(master, Q873_1=="" & Q873_2=="")
good_data <- as.data.frame(sapply(good_data, as.numeric))
good_data <- good_data[,1:870]
average_scores <- colMeans(good_data, na.rm=TRUE)
standard_devs <- colSds(as.matrix(good_data), na.rm=TRUE)

ggplot() + aes(average_scores) + geom_histogram(binwidth = 0.05) + scale_x_continuous(breaks = scales::pretty_breaks(n = 11), limits = c(-0.05, 1.05))


descriptions$average_score <- average_scores
descriptions$score_sd <- standard_devs
descriptions$funding_dummy <- ifelse(descriptions$total_funding>0,1,0)

funded_only <- filter(descriptions, funding_dummy==1)
ggplot(descriptions, aes(average_score, total_funding)) + geom_point()
ggplot(funded_only, aes(average_score)) + geom_histogram(binwidth = 0.05) + scale_x_continuous(breaks = scales::pretty_breaks(n = 11), limits = c(-0.05, 1.05))
ggplot(funded_only, aes(average_score, total_funding)) + geom_point()

## Clean data and wordclouds
company_details <- descriptions$descriptions

# lowercase
company_details <- tolower(company_details)
company_details <- gsub("[\r\n]", " ", company_details)

# Replace @UserName
company_details <- gsub("@\\w+", " ", company_details)

# Remove punctuation
company_details <- gsub("[[:punct:]]+", " ", company_details)

# Remove digits
company_details <- gsub("[[:digit:]]+", " ", company_details)

# Remove links
company_details <- gsub("http\\w+", " ", company_details)

myStopwords <- c(stopwords("en"), "e", "s", "m", "d", "t", "africa", "african", "kenya", "kenyan", "nigeria", "nigerian", "uganda", "ugandan", "ghana", "ghanaian", "www", "com", "will", "can", "dazaar", "co", "propeies", "poal", "staups", "enteainment", "â", "propey")

# Remove Stopwords
company_details <- removeWords(company_details, myStopwords)

# Remove blank spaces at the beginning
company_details <- gsub("^ ", "", company_details)

# Remove blank spaces at the end
company_details <- gsub(" $", "", company_details)

# Remove tabs
company_details <- gsub("[ |\t]{2,}", " ", company_details)
company_details <- gsub("^ *|(?<= ) | *$", "", company_details, perl = TRUE)

## Wordclouds
descriptions$clean_text <- company_details
high_social <- filter(descriptions, average_score>=0.8)
high_commercial  <- filter(descriptions, average_score<=0.2)
middle_scores <- filter(descriptions, average_score>=0.4 & average_score<=0.6)

# library("wordcloud")
# company_corpus <- Corpus(VectorSource(high_social$clean_text))
# wordcloud(company_corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
# 
# company_corpus <- Corpus(VectorSource(high_commercial$clean_text))
# wordcloud(company_corpus,min.freq = 3, scale=c(4,.5), colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 100)
# 
# company_corpus <- Corpus(VectorSource(middle_scores$clean_text))
# wordcloud(company_corpus,min.freq = 3, scale=c(3,.25), colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 75, use.r.layout = FALSE)


## Regularized regression?
library(quanteda)
dtm <- dfm(descriptions$descriptions, removePunct = TRUE)
dtm <- trim(dtm, minCount=4, minDoc=5)
dtm <- as.data.frame(dtm)
dtm$average_score <- descriptions$average_score

library(useful)
theForm <- average_score ~ .
masterX <- build.x(theForm, data=dtm)
masterY <- build.y(theForm, data=dtm)

library(glmnet)
set.seed(102)
glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='gaussian')
plot(glm1, xvar="lambda", label=TRUE)
cv.glm1 <- cv.glmnet(x=masterX,y=masterY,alpha=1,family='gaussian')
plot(cv.glm1)
best_lambda <- cv.glm1$lambda.min
best_lambda
glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='gaussian', lambda=best_lambda)
#coef(glm1)

#coefficient table
ind <- which(coef(glm1) != 0)
df <- data.frame(
    feature=rownames(coef(glm1))[ind],
    coefficient=coef(glm1)[ind]
  )
df <- df[order(df$coefficient),]
kable(df, format = "latex", row.names = FALSE)

## Relationship with funding and score

#covariance test for significance
#library(covTest)
#library(lars)
#lars1 <- lars(masterX, masterY, type="lasso", trace=TRUE, normalize=TRUE, use.Gram = FALSE, max.steps=30)
#plot(lars1)
#cov_lars1 <- covTest(lars1, masterX, masterY)
#cov_lars1