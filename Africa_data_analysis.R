

library(dplyr)
library(ggplot2)
library(matrixStats)
library(tm)
library(knitr)

setwd("/Users/nataliecarlson/Desktop/Tech Hubs/SSA Project Data and Analysis")

descriptions <- read.csv("descriptions.csv", header=TRUE, stringsAsFactors=FALSE)
descriptions <- descriptions[,-1]

master <- read.csv("SSA_Startups_4.csv", header=TRUE, stringsAsFactors=FALSE)

master <- master[-1,13:884]
master[master==2] <- 0
good_data <- filter(master, Q873_1=="" & Q873_2=="")
good_data <- as.data.frame(sapply(good_data, as.numeric))
good_data <- good_data[,1:870]
num_responses <- colSums(good_data!=0, na.rm=TRUE)
average_scores <- colMeans(good_data, na.rm=TRUE)
standard_devs <- colSds(as.matrix(good_data), na.rm=TRUE)

ggplot() + aes(average_scores) + geom_histogram(binwidth = 0.05) + scale_x_continuous(breaks = scales::pretty_breaks(n = 11), limits = c(-0.05, 1.05))


descriptions$average_score <- average_scores
descriptions$score_sd <- standard_devs
descriptions$num_responses <- num_responses
descriptions$funding_dummy <- ifelse(descriptions$total_funding>0,1,0)

funded_only <- filter(descriptions, funding_dummy==1)
ggplot(descriptions, aes(average_score, total_funding)) + geom_point()
ggplot(funded_only, aes(average_score)) + geom_histogram(binwidth = 0.05) + scale_x_continuous(breaks = scales::pretty_breaks(n = 11), limits = c(-0.05, 1.05))
ggplot(funded_only, aes(average_score, total_funding)) + geom_point()
#under five million
ggplot(funded_only, aes(average_score, total_funding)) + geom_point() + ylim(0,5000000)
#under 100k
ggplot(funded_only, aes(average_score, total_funding)) + geom_point() + ylim(25000,2000000)



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

## Matching and adding in category information

#dealing with duplicates
descriptions$dup1 <- duplicated(descriptions[,1], fromLast = TRUE)
descriptions$dup2 <- duplicated(descriptions[,1], fromLast = FALSE)
#all_duplicates <- filter(descriptions, (dup1==TRUE | dup2==TRUE))
#all_duplicates <- all_duplicates[order(all_duplicates$descriptions),]
duplicates <- filter(descriptions, dup1==TRUE)
descriptions <- filter(descriptions, dup1==FALSE)
descriptions$match <- match(descriptions$descriptions, duplicates$descriptions, nomatch=0)
#average scores for dups
for (i in 1:nrow(descriptions)) {
  if (descriptions$dup2[i]==TRUE){
    n <- descriptions$match[i]
    count <- duplicates$num_responses[n] + descriptions$num_responses[i]
    descriptions$average_score[i] <- (duplicates$average_score[n] * (duplicates$num_responses[n]/count)) + (descriptions$average_score[i] * (descriptions$num_responses[i]/count))
    descriptions$num_responses[i] <- count
  }
}

#check <- filter(descriptions, dup2==TRUE)

#Add in categories
categories <- read.csv("category_matrix.csv", header=TRUE, stringsAsFactors=FALSE)
categories <- categories[,-1]
categories$descriptions <- NULL
descriptions_categories <- merge(descriptions, categories, by.x = c("names"), by.y=c("names"), all.x = TRUE, all.y = FALSE)
descriptions_categories$dup1 <- duplicated(descriptions_categories$descriptions, fromLast = TRUE)
descriptions_categories <- filter(descriptions_categories, dup1==FALSE)

## Regularized regression with just the text
library(quanteda)
dtm <- dfm(descriptions$descriptions, removePunct = TRUE)
dtm <- trim(dtm, minCount=5, minDoc=5)
dtm <- as.data.frame(dtm)
dtm$average_score <- descriptions$average_score
#dtm$funding_dummy <- descriptions$funding_dummy
#dtm$total_funding <- descriptions$total_funding

library(useful)
theForm <- average_score ~ .
#theForm <- funding_dummy ~ .
theForm <- total_funding ~ .
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
kable(df, row.names = FALSE)

## Do so with categories and countries included
dtm$country <- descriptions_categories$country
#dtm$funding_dummy <- descriptions$funding_dummy
#dtm$total_funding <- descriptions$total_funding
categories_only <- descriptions_categories[,13:224]
dtm <- cbind(dtm, categories_only)
dtm <- na.omit(dtm)

library(useful)
theForm <- average_score ~ .
#theForm <- funding_dummy ~ .
#theForm <- total_funding ~ .
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
kable(df, row.names = FALSE)


## Relationship with funding and score
drops <- c("total_funding","names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match")
dummy_df<- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]
drops <- c("funding_dummy","names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match")
total_df <- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]
lm1 <- lm(funding_dummy ~ average_score + ., dummy_df)
lm2 <- lm(funding_dummy ~ average_score + I(average_score^2) + ., dummy_df)
lm3 <- lm(total_funding ~ average_score + ., total_df)
lm4 <- lm(total_funding ~ average_score + I(average_score^2) + ., total_df)

#outliers <- quantile( descriptions_categories$total_funding, 0.93)

under_150k <- filter(descriptions_categories, total_funding<=150000)
lm1 <- lm(total_funding ~ average_score + I(average_score^2) + country, under_150k)

## Topic Modeling

library("lda")
library("ggplot2")

corpus <- lexicalize(descriptions$clean_text)


alpha <- 1/22
eta <- 1/22

#set.seed(109)
#set.seed(111)
set.seed(103)
model <- lda.collapsed.gibbs.sampler(corpus$documents, 22, corpus$vocab, 1000, alpha = alpha, eta = eta, compute.log.likelihood=T, trace=0L)
topics <- top.topic.words(model$topics, n=5)
topics

counts <- t(model$document_sums)
proportions <- as.data.frame(counts/(rowSums(counts)))

topics <- sapply(as.data.frame(topics[,1:22]), paste, collapse=", ")
topics <- unname(topics)
names(proportions) <- topics
descriptions_topics <- cbind(descriptions$average_score, proportions)
colnames(descriptions_topics)[1] <- "average_score"
lm1 <- lm(average_score ~ . , descriptions_topics)

library(coefplot)
coefplot(lm1, sort="magnitude")


## TEST FOR NUMBER OF TOPICS BY HARMONIC MEAN?
# library(topicmodels)
# 
# harmonicMean <- function(logLikelihoods, precision=2000L) {
#   library("Rmpfr")
#   llMed <- median(logLikelihoods)
#   as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
#                                        prec = precision) + llMed))))
# }
# 
# # The log-likelihood values are then determined by first fitting the model using for example
# k = 20
# burnin = 1000
# iter = 1000
# keep = 50
# 
# text_dfm <- dfm(descriptions$clean)
# # generate numerous topic models with different numbers of topics
# sequ <- seq(2, 50, 1) # in this case a sequence of numbers from 1 to 50, by ones.
# fitted_many <- lapply(sequ, function(k) LDA(text_dfm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))
# 
# # extract logliks from each topic
# logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
# 
# # compute harmonic means
# hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
# 
# # inspect
# plot(sequ, hm_many, type = "l")
# 
# # compute optimum number of topics
# sequ[which.max(hm_many)]


## RELATIONSHIP BETWEEN TOPICS AND FUNDING
topic_proportions <- as.data.frame(counts/(rowSums(counts)))
descriptions_topics <- cbind(descriptions, topic_proportions)
descriptions_topics$country <- descriptions_categories$country

drops <- c("total_funding","names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match")
dummy_df<- descriptions_topics[ , !(names(descriptions_topics) %in% drops)]
drops <- c("average_score", "funding_dummy","names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match")
total_df <- descriptions_topics[ , !(names(descriptions_topics) %in% drops)]
lm1 <- glm(funding_dummy ~ average_score + I(average_score^2) + ., data=dummy_df, family = "binomial")
lm3 <- lm(total_funding ~ ., total_df)

# topic_interactions <- model.matrix(~(.)^2, topic_proportions)
# descriptions_interactions <- cbind(descriptions, topic_interactions)
# descriptions_interactions$country <- descriptions_categories$country
# drops <- c("average_score", "total_funding","names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match")
# dummy_df<- descriptions_interactions[ , !(names(descriptions_interactions) %in% drops)]
# lm1 <- lm(funding_dummy ~ ., data=dummy_df)

# interaction between score and topics
