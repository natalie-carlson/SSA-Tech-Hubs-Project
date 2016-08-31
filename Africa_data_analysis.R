
library(plyr)
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
#good_data <- filter(master, Q873_1=="" & Q873_2=="")
good_data <- master
good_data <- as.data.frame(sapply(good_data, as.numeric))
good_data <- good_data[,1:870]
num_responses <- colSums(good_data!=0, na.rm=TRUE)
average_scores <- colMeans(good_data, na.rm=TRUE)
standard_devs <- colSds(as.matrix(good_data), na.rm=TRUE)

ggplot() + aes(average_scores) + geom_histogram(binwidth = 0.1) + scale_x_continuous(breaks = scales::pretty_breaks(n = 11), limits = c(-0.05, 1.05))


descriptions$average_score <- average_scores
descriptions$score_sd <- standard_devs
descriptions$num_responses <- num_responses
descriptions$funding_dummy <- ifelse(descriptions$total_funding>0,1,0)

ggplot(descriptions, aes(x=average_scores, fill=factor(funding_dummy))) + geom_histogram(binwidth = 0.05) + scale_x_continuous(breaks = scales::pretty_breaks(n = 11), limits = c(-0.05, 1.05))

#drop those with no responses
descriptions <- filter(descriptions, num_responses!=0)


library(gtools)
bin_descriptions <- data.frame(descriptions, bin=cut(descriptions$average_score, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), include.lowest=TRUE))
ggplot(bin_descriptions, aes(x=bin, y=funding_dummy)) + stat_summary(fun.y="mean", geom="bar") + xlab("Less Social <--- Average mTurk Score ---> More Social") + ylab("Proportion of Firms Funded")



funded_only <- filter(descriptions, funding_dummy==1)
ggplot(descriptions, aes(average_score, total_funding)) + geom_point()
ggplot(funded_only, aes(average_score)) + geom_histogram(binwidth = 0.05) + scale_x_continuous(breaks = scales::pretty_breaks(n = 11), limits = c(-0.05, 1.05))
ggplot(funded_only, aes(average_score, total_funding)) + geom_point()

ggplot(funded_only, aes(average_score, total_funding)) + geom_point() + ylim(0,10000000)
#under 100k
ggplot(funded_only, aes(average_score, total_funding)) + geom_point() + ylim(100000,2000000)
#outliers
outliers <- quantile(descriptions$total_funding, 0.99)
ggplot(funded_only, aes(average_score, total_funding)) + geom_point() + ylim(0,outliers)

## CENSORING ABSURD FUNDING AMOUNTS
descriptions <- filter(descriptions, total_funding<outliers)


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

myStopwords <- c(stopwords(), "e", "s", "m", "d", "t", "africa", "african", "kenya", "kenyan", "nigeria", "nigerian", "uganda", "ugandan", "ghana", "ghanaian", "www", "com", "will", "can", "dazaar", "co", "propeies", "poal", "staups", "enteainment", "â", "pistis", "also", "right")

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
categories <- read.csv("category_matrix_3.csv", header=TRUE, stringsAsFactors=FALSE)
categories <- categories[,-1]
categories$descriptions <- NULL
descriptions_categories <- merge(descriptions, categories, by.x = c("names"), by.y=c("names"), all.x = TRUE, all.y = FALSE)
descriptions_categories$dup1 <- duplicated(descriptions_categories$descriptions, fromLast = TRUE)
descriptions_categories <- filter(descriptions_categories, dup1==FALSE)

write.csv(descriptions_categories, "category_matrix_order.csv")

## Regularized regression with just the text
library(quanteda)
dtm <- dfm(descriptions$descriptions, removePunct = TRUE, ignoredFeatures = c(stopwords(), "â"))
dtm <- trim(dtm, minCount=5, minDoc=5)
dtm <- as.data.frame(dtm)
dtm$average_score <- descriptions$average_score
#dtm$funding_dummy <- descriptions$funding_dummy
#dtm$total_funding <- descriptions$total_funding

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

## Do so with categories and countries included
dtm$country <- descriptions_categories$country
#dtm$funding_dummy <- descriptions$funding_dummy
#dtm$total_funding <- descriptions$total_funding
categories_only <- descriptions_categories[,13:231]
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
#glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
plot(glm1, xvar="lambda", label=TRUE)
cv.glm1 <- cv.glmnet(x=masterX,y=masterY,alpha=1,family='gaussian')
#cv.glm1 <- cv.glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
plot(cv.glm1)
best_lambda <- cv.glm1$lambda.min
best_lambda
glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='gaussian', lambda=best_lambda)
#glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial', lambda=best_lambda)
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
drops <- c("total_funding","names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "investors", "investor_type", "funding_type", "funding_rounds")
dummy_df<- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]
drops <- c("funding_dummy","names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "investors", "investor_type", "funding_type", "funding_rounds")
total_df <- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]
lm1 <-lm(total_funding ~ average_score + I(average_score^2) + country, total_df)
lm3 <- lm(total_funding ~ average_score + I(average_score^2) + ., total_df)
binomial1 <- glm(funding_dummy ~ average_score + I(average_score^2) + country, dummy_df, family="binomial")
binomial2 <- glm(funding_dummy ~ average_score  + I(average_score^2) + Software + Mobile + E.Commerce + Education + Internet + country, dummy_df, family="binomial")
binomial3 <- glm(funding_dummy ~ average_score + I(average_score^2) + ., dummy_df, family="binomial")

#outliers <- quantile(descriptions_categories$total_funding, 0.99)
under_150k <- filter(descriptions_categories, total_funding<25000)
lm1 <- lm(total_funding ~ average_score + I(average_score^2) + country, under_150k)

## TOPIC MODELING

library("lda")
library("ggplot2")

corpus <- lexicalize(descriptions$clean_text)


alpha <- 1/22
eta <- 1/22

set.seed(110)
#set.seed(104)
#set.seed(103)
model <- lda.collapsed.gibbs.sampler(corpus$documents, 22, corpus$vocab, 1000, alpha = alpha, eta = eta, compute.log.likelihood=T, trace=0L)
topics <- top.topic.words(model$topics, n=5)
topics

counts <- t(model$document_sums)
proportions <- as.data.frame(counts/(rowSums(counts)))

topics <- sapply(as.data.frame(topics[,1:22]), paste, collapse=", ")
topics <- unname(topics)
names(proportions) <- topics
for (i in 1:length(proportions)) {
  names(proportions)[i] <- paste(i, topics[i], sep=": " )
}

descriptions_topics <- cbind(descriptions$average_score, proportions)
colnames(descriptions_topics)[1] <- "average_score"
lm1 <- lm(average_score ~ . , descriptions_topics)

library(coefplot)
coefplot(lm1, sort="magnitude") + ylab("") + ggtitle("Coefficients from Regression of mTurk Score on Topics") + theme(plot.title = element_text(size = rel(1), hjust = 0))

# Most prevalent topics
average_prop <- as.data.frame(colMeans(proportions))
average_prop$proportion <- average_prop$`colMeans(proportions)`
average_prop$`colMeans(proportions)` <- NULL
ggplot(average_prop, aes(x=reorder(rownames(average_prop), proportion), y=proportion)) + geom_bar(stat='identity') + coord_flip() + ylab("") + ggtitle("Topics by Prevalence") + theme(plot.title = element_text(size = rel(1), hjust = 0), axis.title.y=element_blank())


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
# text_dfm <- dfm(descriptions$clean_text)
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
drops <- c("funding_dummy","names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match")
total_df <- descriptions_topics[ , !(names(descriptions_topics) %in% drops)]
lm1 <- glm(funding_dummy ~ average_score + I(average_score^2) + ., data=dummy_df, family = "binomial")
dummy_df$average_score <- NULL
lm2 <- glm(funding_dummy ~ ., data = dummy_df, family = "binomial")
lm3 <- lm(total_funding ~ average_score + I(average_score^2) + ., total_df)

# interaction between score and topics
score_topics <- data.frame(matrix(0, nrow=length(descriptions), ncol=length(topic_proportions)))
colnames(score_topics) <- paste("Score", colnames(topic_proportions), sep = "_")

for (i in 1:nrow(descriptions)) {
  for (j in 1:ncol(topic_proportions)) {
    score_topics[i,j] <- descriptions$average_score[i]*topic_proportions[i,j]
  }
}


## LASSO ON ALL TOPIC INTERACTIONS AND FUNDING/SCORE
topic_interactions <- model.matrix(~(.)^2, topic_proportions)
descriptions_interactions <- cbind(descriptions, topic_interactions)
descriptions_interactions$country <- descriptions_categories$country
drops <- c("average_score", "total_funding", "names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match")
dummy_df<- descriptions_interactions[ , !(names(descriptions_interactions) %in% drops)]

#theForm <- total_funding ~ .
theForm <- funding_dummy ~ .
#theForm <- total_funding ~ .
masterX <- build.x(theForm, data=dummy_df)
masterY <- build.y(theForm, data=dummy_df)

# set.seed(102)
# glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
# plot(glm1, xvar="lambda", label=TRUE)
# cv.glm1 <- cv.glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
# plot(cv.glm1)
# best_lambda <- cv.glm1$lambda.min
# best_lambda
# glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial', lambda=best_lambda)
#coef(glm1)

#coefficient table
# ind <- which(coef(glm1) != 0)
# df <- data.frame(
#   feature=rownames(coef(glm1))[ind],
#   coefficient=coef(glm1)[ind]
# )
# df <- df[order(df$coefficient),]
# kable(df, row.names = FALSE)
# 
# binomial1 <- glm(funding_dummy ~ ., dummy_df, family="binomial")

## Looking at funding specifics  -- number of rounds, venture or private equity, etc. 
library(mpath)
library(MASS)

categories_topics <- cbind(descriptions_categories, topic_proportions)
categories_topics$two_or_more <- ifelse(categories_topics$funding_rounds>1, 1, 0)
funded_only <- filter(categories_topics, funding_dummy==1)
#poisson1 <- glm(funding_rounds ~ average_score + country, data=funded_only, family = "poisson")
#pchisq(poisson1$deviance, df=poisson1$df.residual, lower.tail=FALSE)

binomial1 <- glm(two_or_more ~ average_score + I(average_score^2) + country, data = categories_topics, family = "binomial")
binomial2 <- glm(two_or_more ~ average_score + country, data = funded_only, family = "binomial")

#binomial3 <- glm(two_or_more ~ average_score + I(average_score^2) + country + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22, data = categories_topics, family = "binomial")
#binomial4 <- glm(two_or_more ~ average_score + I(average_score^2) + country + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22, data = funded_only, family = "binomial")

funding_types <- lapply(descriptions_categories$funding_type, function(x) strsplit(x, "\\|"))
all_funding_types <- unique(unlist(funding_types))
funding_matrix <- data.frame(matrix(0, nrow=length(funding_types), ncol=length(all_funding_types)))
colnames(funding_matrix) <- all_funding_types

for (i in 1:nrow(funding_matrix)) {
  for (j in 1:length(all_funding_types)){
    if (all_funding_types[j] %in% funding_types[[i]][[1]]) funding_matrix[i,j] <- 1 
  }
}

funding_matrix$venture_or_pe <- ifelse(funding_matrix$venture==1 | funding_matrix$private_equity==1, 1, 0)
descriptions_categories$venture_or_pe <- funding_matrix$venture_or_pe
descriptions_categories$seed <- funding_matrix$seed
descriptions_categories$equity_crowdfunding <- funding_matrix$equity_crowdfunding
descriptions_categories$venture <- funding_matrix$venture
funded_only <- filter(descriptions_categories, funding_dummy==1)
binomial1 <- glm(venture_or_pe ~ average_score + I(average_score^2) + country, data = descriptions_categories, family = "binomial")

drops <- c("total_funding", "funding_dummy", "names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "venture", "equity_crowdfunding", "venture_or_pe", "funding_type", "funding_rounds", "seed_funding", "venture_pe_funding", "investors")
seed_df<- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]
binomial2 <- glm(seed ~ average_score + I(average_score^2) + country, data = seed_df, family = "binomial")
binomial3 <- glm(seed ~ average_score + I(average_score^2) + country + ., data = seed_df, family = "binomial")

one_round <- filter(descriptions_categories, funding_rounds<2)
binomial1 <- glm(funding_dummy ~ average_score + I(average_score^2) + country, data = one_round, family = "binomial")


seed_only <- filter(descriptions_categories, seed==1 & funding_rounds==1)
outliers <- quantile(seed_only$total_funding, 0.99)
seed_only <- filter(seed_only, total_funding<outliers)
ggplot(seed_only, aes(average_score, total_funding)) + geom_point()
ggplot(seed_only, aes(average_score, total_funding)) + geom_point() + ylim(0,500000)
venture_pe_only <- filter(descriptions_categories, venture_or_pe==1)
#outliers <- quantile(venture_pe_only$total_funding, 0.99)
#venture_pe_only <- filter(venture_pe_only, total_funding<outliers)
ggplot(venture_pe_only, aes(average_score, total_funding)) + geom_point()
ggplot(venture_pe_only, aes(average_score, total_funding)) + geom_point() + ylim(0,20000000)


#only one funding round
one_round <- filter(descriptions_categories, funding_rounds==1)
ggplot(one_round, aes(average_score, total_funding)) + geom_point()
ggplot(one_round, aes(average_score, total_funding)) + geom_point() + ylim(0,100000)

#multiple rounds
two_or_more <- filter(categories_topics, two_or_more==1)
ggplot(two_or_more, aes(average_score, total_funding)) + geom_point()
ggplot(two_or_more, aes(average_score, total_funding)) + geom_point() + ylim(0,100000)



#amount of venture/pe and seed funding
#descriptions_categories$venture_pe_funding <- ifelse(descriptions_categories$venture_or_pe==1, descriptions_categories$total_funding, 0) 
#descriptions_categories$seed_funding <- ifelse(descriptions_categories$venture_or_pe==0 & descriptions_categories$seed==1, descriptions_categories$total_funding, 0) 
#funded_only <- filter(descriptions_categories, funding_dummy==1)
#venture_pe_only <- filter(descriptions_categories, venture_or_pe==1)
lm1 <- lm(total_funding ~ average_score + country, data = venture_pe_only)
#drops <- c("total_funding", "funding_dummy", "names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "venture", "seed", "equity_crowdfunding", "venture_or_pe", "funding_type", "funding_rounds")
#VPE_df<- venture_pe_only[ , !(names(venture_pe_only) %in% drops)]
#lm1 <- lm(venture_pe_funding ~ average_score + country + ., data = VPE_df)
#seed_only <- filter(descriptions_categories, seed==1)
lm1 <- lm(total_funding ~ average_score + I(average_score^2) + country, data = seed_only)

#relationship with topics?
categories_topics$venture_or_pe <- descriptions_categories$venture_or_pe
categories_topics$seed <- descriptions_categories$seed
seed_only <- filter(categories_topics, seed==1)
outliers <- quantile(seed_only$total_funding, 0.99)
seed_only <- filter(seed_only, total_funding<outliers)
venture_pe_only <- filter(categories_topics, venture_or_pe==1)
lm1 <- lm(total_funding ~ country + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22, data = venture_pe_only)
lm1 <- lm(total_funding ~ average_score + country + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22, data = seed_only)
binomial1 <- glm(seed ~ average_score + I(average_score^2) + country + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22, data = categories_topics, family="binomial")
binomial1 <- glm(venture_or_pe ~ average_score + country + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22, data = categories_topics, family="binomial")


##regress funding on score/topics 
score_topics_df <- cbind(topic_proportions, score_topics)
score_topics_df$average_score <- descriptions$average_score 
score_topics_df$country <- descriptions_categories$country 
score_topics_df$total_funding<- descriptions_categories$total_funding


#score_topics_df <- filter(score_topics_df, total_funding<10000000)
lm1 <- lm(total_funding ~ ., score_topics_df)
score_topics_df$total_funding <- NULL
score_topics_df$funding_dummy <- descriptions_categories$funding_dummy
binomial1 <- glm(funding_dummy ~ ., score_topics_df, family="binomial")
score_topics_df$funding_dummy <- NULL
score_topics_df$venture_or_pe <- categories_topics$venture_or_pe
binomial2 <- glm(venture_or_pe ~ ., score_topics_df, family="binomial")
score_topics_df$venture_or_pe <- NULL
score_topics_df$seed <- categories_topics$seed
binomial2 <- glm(seed ~ ., score_topics_df, family="binomial")

## Topics interactions (most with least social)
topics_interactions <- topic_proportions
topics_interactions$V3.V1 <- topics_interactions$V3*topics_interactions$V1
topics_interactions$V3.V2 <- topics_interactions$V3*topics_interactions$V2
topics_interactions$V3.V4 <- topics_interactions$V3*topics_interactions$V4
topics_interactions$V3.V5 <- topics_interactions$V3*topics_interactions$V5
topics_interactions$V3.V6 <- topics_interactions$V3*topics_interactions$V6
topics_interactions$V3.V7 <- topics_interactions$V3*topics_interactions$V7
topics_interactions$V3.V8 <- topics_interactions$V3*topics_interactions$V8
topics_interactions$V3.V9 <- topics_interactions$V3*topics_interactions$V9
topics_interactions$V3.V10 <- topics_interactions$V3*topics_interactions$V10
topics_interactions$V3.V11 <- topics_interactions$V3*topics_interactions$V11
topics_interactions$V3.V12 <- topics_interactions$V3*topics_interactions$V12
topics_interactions$V3.V13 <- topics_interactions$V3*topics_interactions$V13
topics_interactions$V3.V14 <- topics_interactions$V3*topics_interactions$V14
topics_interactions$V3.V15 <- topics_interactions$V3*topics_interactions$V15
topics_interactions$V3.V16 <- topics_interactions$V3*topics_interactions$V16
topics_interactions$V3.V17 <- topics_interactions$V3*topics_interactions$V17
topics_interactions$V3.V18 <- topics_interactions$V3*topics_interactions$V18
topics_interactions$V3.V19 <- topics_interactions$V3*topics_interactions$V19
topics_interactions$V3.V20 <- topics_interactions$V3*topics_interactions$V20
topics_interactions$V3.V21 <- topics_interactions$V3*topics_interactions$V21
topics_interactions$V3.V22 <- topics_interactions$V3*topics_interactions$V22

topics_interactions$funding_dummy <- descriptions_categories$funding_dummy
topics_interactions$country <- descriptions_categories$country
binomial1 <- glm(funding_dummy ~ ., topics_interactions, family="binomial")
topic_proportions$funding_dummy <- descriptions_categories$funding_dummy
binomial2 <- glm(funding_dummy ~ ., topic_proportions, family="binomial")

topics_interactions <- topic_proportions
topics_interactions$V14.V1 <- topics_interactions$V14*topics_interactions$V1
topics_interactions$V14.V2 <- topics_interactions$V14*topics_interactions$V2
topics_interactions$V14.V3 <- topics_interactions$V14*topics_interactions$V3
topics_interactions$V14.V4 <- topics_interactions$V14*topics_interactions$V4
topics_interactions$V14.V5 <- topics_interactions$V14*topics_interactions$V5
topics_interactions$V14.V6 <- topics_interactions$V14*topics_interactions$V6
topics_interactions$V14.V7 <- topics_interactions$V14*topics_interactions$V7
topics_interactions$V14.V8 <- topics_interactions$V14*topics_interactions$V8
topics_interactions$V14.V9 <- topics_interactions$V14*topics_interactions$V9
topics_interactions$V14.V10 <- topics_interactions$V14*topics_interactions$V10
topics_interactions$V14.V11 <- topics_interactions$V14*topics_interactions$V11
topics_interactions$V14.V12 <- topics_interactions$V14*topics_interactions$V12
topics_interactions$V14.V13 <- topics_interactions$V14*topics_interactions$V13
topics_interactions$V14.V15 <- topics_interactions$V14*topics_interactions$V15
topics_interactions$V14.V16 <- topics_interactions$V14*topics_interactions$V16
topics_interactions$V14.V17 <- topics_interactions$V14*topics_interactions$V17
topics_interactions$V14.V18 <- topics_interactions$V14*topics_interactions$V18
topics_interactions$V14.V19 <- topics_interactions$V14*topics_interactions$V19
topics_interactions$V14.V20 <- topics_interactions$V14*topics_interactions$V20
topics_interactions$V14.V21 <- topics_interactions$V14*topics_interactions$V21
topics_interactions$V14.V22 <- topics_interactions$V14*topics_interactions$V22

topics_interactions$funding_dummy <- descriptions_categories$funding_dummy
topics_interactions$country <- descriptions_categories$country
binomial1 <- glm(funding_dummy ~ ., topics_interactions, family="binomial")


#what distinguishes low from middle scores?
library(quanteda)
dtm <- dfm(descriptions$descriptions, removePunct = TRUE, ignoredFeatures = c(stopwords(), "â"))
dtm <- trim(dtm, minCount=5, minDoc=5)
dtm <- as.data.frame(dtm)
dtm$average_score <- descriptions$average_score
median <- quantile(dtm$average_score, 0.50)
dtm_train <- filter(dtm, average_score<median)

library(useful)
theForm <- average_score ~ .
masterX <- build.x(theForm, data=dtm_train)
masterY <- build.y(theForm, data=dtm_train)

library(glmnet)
set.seed(102)
#glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
#plot(glm1, xvar="lambda", label=TRUE)
cv.glm1 <- cv.glmnet(x=masterX,y=masterY,alpha=1,family='gaussian')
plot(cv.glm1)
best_lambda <- cv.glm1$lambda.min
glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='gaussian', lambda=best_lambda)

#coefficient table
ind <- which(coef(glm1) != 0)
df <- data.frame(
  feature=rownames(coef(glm1))[ind],
  coefficient=coef(glm1)[ind]
)
df <- df[order(df$coefficient),]
kable(df, row.names = FALSE)

#what distinguishes middle from high scores?
library(quanteda)
dtm <- dfm(descriptions$descriptions, removePunct = TRUE, ignoredFeatures = c(stopwords(), "â"))
dtm <- trim(dtm, minCount=5, minDoc=5)
dtm <- as.data.frame(dtm)
dtm$average_score <- descriptions$average_score
median <- quantile(dtm$average_score, 0.50)
dtm_train <- filter(dtm, average_score>median)

library(useful)
theForm <- average_score ~ .
masterX <- build.x(theForm, data=dtm_train)
masterY <- build.y(theForm, data=dtm_train)

library(glmnet)
set.seed(102)
#glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
#plot(glm1, xvar="lambda", label=TRUE)
cv.glm1 <- cv.glmnet(x=masterX,y=masterY,alpha=1,family='gaussian')
plot(cv.glm1)
best_lambda <- cv.glm1$lambda.min
glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='gaussian', lambda=best_lambda)

#coefficient table
ind <- which(coef(glm1) != 0)
df <- data.frame(
  feature=rownames(coef(glm1))[ind],
  coefficient=coef(glm1)[ind]
)
df <- df[order(df$coefficient),]
kable(df, row.names = FALSE)


## Funding by geographic source
# geographic_source <- read.csv("investors.csv", header=TRUE, stringsAsFactors=FALSE)
# descriptions_categories <- join(descriptions_categories, geographic_source, by="investors")
# descriptions_categories$african_funding <- ifelse(descriptions_categories$investor_type=="african", 1, 0)
# descriptions_categories$international_funding <- ifelse(descriptions_categories$investor_type=="international", 1, 0)
# have_funding_source <- filter(descriptions_categories, african_funding==1 | international_funding==1)
# ggplot(have_funding_source, aes(average_score, total_funding, color=investor_type)) + geom_point() + ylim(0,50000)



## CLASSIFICATION ANALYSIS

# #classify bottom 20% as 0 and top 20% as 1 for training
# library(quanteda)
# dtm <- dfm(descriptions$descriptions, removePunct = TRUE)
# dtm <- trim(dtm, minCount=5, minDoc=5)
# dtm <- as.data.frame(dtm)
# dtm$average_score <- descriptions$average_score
# bottom_quantile <- quantile(dtm$average_score, 0.20)
# top_quantile <- quantile(dtm$average_score, 0.80)
# dtm_train_full <- filter(dtm, average_score<bottom_quantile | average_score>top_quantile)
# dtm_test_full <- filter(dtm, average_score>=bottom_quantile & average_score<=top_quantile)
# dtm_train <- dtm_train_full
# dtm_test <- dtm_test_full
# dtm_train$training_var <- ifelse(dtm_train$average_score<bottom_quantile, 0, 1)
# dtm_test$training_var <- NA
# dtm_train$average_score <- NULL
# dtm_test$average_score <- NULL
# 
# library(useful)
# theForm <- training_var ~ .
# masterX <- build.x(theForm, data=dtm_train)
# masterY <- build.y(theForm, data=dtm_train)
# 
# library(glmnet)
# set.seed(102)
# #glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
# #plot(glm1, xvar="lambda", label=TRUE)
# cv.glm1 <- cv.glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
# plot(cv.glm1)
# best_lambda <- cv.glm1$lambda.min
# glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial', lambda=best_lambda)
# 
# #coefficient table
# ind <- which(coef(glm1) != 0)
# df <- data.frame(
#   feature=rownames(coef(glm1))[ind],
#   coefficient=coef(glm1)[ind]
# )
# df <- df[order(df$coefficient),]
# kable(df, row.names = FALSE)
# 
# prediction <- predict(glm1, as.matrix(dtm), s=best_lambda, type="class")
# probability <- predict(glm1, as.matrix(dtm), s=best_lambda, type="response")
# dtm$prediction <- as.vector(prediction)
# dtm$prob <- as.vector(probability)
# 
# descriptions_categories$prob <- dtm$prob
# test_data_only <- filter(descriptions_categories, average_score>=bottom_quantile & average_score<=top_quantile)
# binomial1 <- glm(funding_dummy ~ prob + country, test_data_only, family="binomial")
# binomial1 <- glm(funding_dummy ~ prob + country, descriptions_categories, family="binomial")
# drops <- c("total_funding", "names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "venture", "equity_crowdfunding", "venture_or_pe", "funding_type", "funding_rounds", "seed_funding", "venture_pe_funding", "investors", "seed")
# dummy_df<- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]
# binomial1 <- glm(funding_dummy ~ prob + country + ., dummy_df, family="binomial")


#split at median for training instead
# library(quanteda)
# dtm <- dfm(descriptions$descriptions, removePunct = TRUE, ignoredFeatures = c(stopwords(), "â"))
# dtm <- trim(dtm, minCount=5, minDoc=5)
# dtm <- as.data.frame(dtm)
# dtm$average_score <- descriptions$average_score
# median <- quantile(dtm$average_score, 0.50)
# dtm$training_var <- ifelse(dtm$average_score<median, 0, 1)
# library(caret)
# set.seed(102)
# Train <- createDataPartition(dtm$training_var, p=0.5, list=FALSE)
# dtm_train <- dtm[ Train, ]
# dtm_test <- dtm[ -Train, ]
# dtm_train$average_score <- NULL
# dtm_test$average_score <- NULL
# 
# 
# library(useful)
# theForm <- training_var ~ .
# masterX <- build.x(theForm, data=dtm_test)
# masterY <- build.y(theForm, data=dtm_test)
# 
# library(glmnet)
# set.seed(102)
# #glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
# #plot(glm1, xvar="lambda", label=TRUE)
# cv.glm1 <- cv.glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
# plot(cv.glm1)
# best_lambda <- cv.glm1$lambda.min
# glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial', lambda=best_lambda)
# 
# #coefficient table
# ind <- which(coef(glm1) != 0)
# df <- data.frame(
#   feature=rownames(coef(glm1))[ind],
#   coefficient=coef(glm1)[ind]
# )
# df <- df[order(df$coefficient),]
# kable(df, row.names = FALSE)
# 
# prediction <- predict(glm1, as.matrix(dtm_test), s=best_lambda, type="class")
# probability <- predict(glm1, as.matrix(dtm_test), s=best_lambda, type="response")
# dtm_test$prediction <- as.vector(prediction)
# dtm_test$prob <- as.vector(probability)
# dtm_test$confidence <- abs(dtm_test$prob - .5)
# 
# 
# descriptions_test <-  descriptions_categories[ -Train, ]
# descriptions_test$confidence <- dtm_test$confidence
# descriptions_test$prob <- dtm_test$prob
# descriptions_test$prediction <- dtm_test$prediction
# binomial1 <- glm(funding_dummy ~ confidence + country, descriptions_test, family="binomial")
# 
# 
# 
# drops <- c("total_funding", "names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "venture", "equity_crowdfunding", "venture_or_pe", "funding_type", "funding_rounds", "seed_funding", "venture_pe_funding", "investors", "seed")
# dummy_df<- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]
# binomial1 <- glm(funding_dummy ~ prob + country + ., dummy_df, family="binomial")


# quartile25 <- quantile(descriptions_categories$average_score, 0.25)
# quartile50 <- quantile(descriptions_categories$average_score, 0.50)
# quartile75 <- quantile(descriptions_categories$average_score, 0.75)
# 
# descriptions_categories$first_quartile <- ifelse(descriptions_categories$average_score>=quartile75, 1, 0)
# descriptions_categories$second_quartile <- ifelse(descriptions_categories$average_score<quartile75 & descriptions_categories$average_score>=quartile50, 1, 0)
# descriptions_categories$third_quartile <- ifelse(descriptions_categories$average_score<quartile50 & descriptions_categories$average_score>=quartile25, 1, 0)
# 
# descriptions_categories$middle_score <- ifelse(descriptions_categories$average_score<=0.6 & descriptions_categories$average_score>=0.4, 1, 0)
# # 
# binomial1 <- glm(funding_dummy ~ middle_score + country, descriptions_categories, family="binomial")
# drops <- c("total_funding", "names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "venture", "equity_crowdfunding", "venture_or_pe", "funding_type", "funding_rounds", "seed_funding", "venture_pe_funding", "investors", "seed", "prob")
# dummy_df<- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]
# binomial1 <- glm(funding_dummy ~ first_quartile + second_quartile + third_quartile + country + ., dummy_df, family="binomial")


#what distinguishes low from middle scores?
library(quanteda)
dtm <- dfm(descriptions$descriptions, removePunct = TRUE, ignoredFeatures = c(stopwords(), "â"))
dtm <- trim(dtm, minCount=5, minDoc=5)
dtm <- as.data.frame(dtm)
dtm$average_score <- descriptions$average_score
bottom_20 <- quantile(dtm$average_score, 0.20)
middle_40 <- quantile(dtm$average_score, 0.40)
middle_60 <- quantile(dtm$average_score, 0.60)
dtm_train <- filter(dtm, average_score<bottom_20 | (average_score>middle_40 & average_score<middle_60))
dtm_train$training_var <- ifelse(dtm_train$average_score<bottom_20, 0, 1)
dtm_train$average_score <- NULL

library(useful)
theForm <- training_var ~ .
masterX <- build.x(theForm, data=dtm_train)
masterY <- build.y(theForm, data=dtm_train)

library(glmnet)
set.seed(102)
#glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
#plot(glm1, xvar="lambda", label=TRUE)
cv.glm1 <- cv.glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
plot(cv.glm1)
best_lambda <- cv.glm1$lambda.min
glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial', lambda=best_lambda)

#coefficient table
ind <- which(coef(glm1) != 0)
df <- data.frame(
  feature=rownames(coef(glm1))[ind],
  coefficient=coef(glm1)[ind]
)
df <- df[order(df$coefficient),]
kable(df, row.names = FALSE)

#what distinguishes middle from high scores?
library(quanteda)
dtm <- dfm(descriptions$descriptions, removePunct = TRUE, ignoredFeatures = c(stopwords(), "â"))
dtm <- trim(dtm, minCount=5, minDoc=5)
dtm <- as.data.frame(dtm)
dtm$average_score <- descriptions$average_score
top_20 <- quantile(dtm$average_score, 0.80)
middle_40 <- quantile(dtm$average_score, 0.40)
middle_60 <- quantile(dtm$average_score, 0.60)
dtm_train <- filter(dtm, average_score>top_20 | (average_score>middle_40 & average_score<middle_60))
dtm_train$training_var <- ifelse(dtm_train$average_score>top_20, 1, 0)
dtm_train$average_score <- NULL

library(useful)
theForm <- training_var ~ .
masterX <- build.x(theForm, data=dtm_train)
masterY <- build.y(theForm, data=dtm_train)

library(glmnet)
set.seed(102)
#glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
#plot(glm1, xvar="lambda", label=TRUE)
cv.glm1 <- cv.glmnet(x=masterX,y=masterY,alpha=1,family='binomial')
plot(cv.glm1)
best_lambda <- cv.glm1$lambda.min
glm1<-glmnet(x=masterX,y=masterY,alpha=1,family='binomial', lambda=best_lambda)

#coefficient table
ind <- which(coef(glm1) != 0)
df <- data.frame(
  feature=rownames(coef(glm1))[ind],
  coefficient=coef(glm1)[ind]
)
df <- df[order(df$coefficient),]
kable(df, row.names = FALSE)

