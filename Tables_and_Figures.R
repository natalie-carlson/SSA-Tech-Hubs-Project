
library(plyr)
library(dplyr)
library(ggplot2)
library(matrixStats)
library(tm)
library(knitr)

## SET DIRECTORY HERE
setwd("/Users/nataliecarlson/Desktop/Tech Hubs/SSA Project Data and Analysis")

#################################################
# DATA PROCESSING
#################################################

## READ IN MAIN DATA
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

descriptions$average_score <- average_scores
descriptions$score_sd <- standard_devs
descriptions$num_responses <- num_responses
descriptions$funding_dummy <- ifelse(descriptions$total_funding>0,1,0)

#drop those with no responses
descriptions <- filter(descriptions, num_responses!=0)

#drop outliers
outliers <- quantile(descriptions$total_funding, 0.99)
descriptions <- filter(descriptions, total_funding<outliers)

# CLEAN TEXT FOR TOPIC MODELING
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

descriptions$clean_text <- company_details

## REMOVE DUPLICATES
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

## CATEGORIES
categories <- read.csv("category_matrix_3.csv", header=TRUE, stringsAsFactors=FALSE)
categories <- categories[,-1]
categories$descriptions <- NULL
descriptions_categories <- merge(descriptions, categories, by.x = c("names"), by.y=c("names"), all.x = TRUE, all.y = FALSE)
descriptions_categories$dup1 <- duplicated(descriptions_categories$descriptions, fromLast = TRUE)
descriptions_categories <- filter(descriptions_categories, dup1==FALSE)


#################################################
# TABLES AND FIGURES
#################################################

# FIGURE X: PROPORTION OF FIRMS FUNDED BY SCORE
bin_descriptions <- data.frame(descriptions, bin=cut(descriptions$average_score, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), include.lowest=TRUE))
ggplot(bin_descriptions, aes(x=bin, y=funding_dummy)) + stat_summary(fun.y="mean", geom="bar") + xlab("Less Social <--- Average mTurk Score ---> More Social") + ylab("Proportion of Firms Funded")


#TABLE X: Words selected by LASSO model for predicting mTurk score
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
  Feature=rownames(coef(glm1))[ind],
  Coefficient=coef(glm1)[ind]
)
df <- df[order(df$Coefficient),]
library(xtable)

xtable(df, format="latex", include.rownames = FALSE)

coef_table <- xtable(df, format="latex")
print(coef_table, include.rownames = FALSE, size="small")


## TABLE X: LIKELIHOOD OF GETTING FUNDED BASED ON SCORE
library(stargazer)
drops <- c("total_funding","names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "investors", "investor_type", "funding_type", "funding_rounds")
dummy_df<- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]

binomial1 <- glm(funding_dummy ~ average_score + I(average_score^2) + country, dummy_df, family="binomial")
binomial2 <- glm(funding_dummy ~ average_score  + I(average_score^2) + country + Software + Mobile + E.Commerce + Education, dummy_df, family="binomial")
binomial3 <- glm(funding_dummy ~ average_score + I(average_score^2) + country + Software + Mobile + E.Commerce + Education + ., dummy_df, family="binomial")
stargazer(binomial1, binomial2, binomial3, title="Logistic Regression of Funding Dummy on mTurk Score", dep.var.labels="", dep.var.caption = "Funding indicator", covariate.labels = c("mTurk Score", "mTurk Score*2", "Kenya", "Nigeria", "Uganda", "Software", "Mobile", "E-Commerce", "Education"), column.labels=c("Model 1", "Model 2", "Model 3"), omit =c(11:224), no.space = T, digits = 2, omit.stat=c("aic"), size="footnotesize")


## TABLE X: LIKELIHOOD OF PARTICULAR TYPES OF FUNDING
# Process funding types
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

one_round <- filter(descriptions_categories, funding_rounds<2)
drops <- c("total_funding", "seed", "names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "venture_or_pe", "funding_type", "funding_rounds", "investors", "two_or_more")
one_round<- one_round[ , !(names(one_round) %in% drops)]
binomial1 <- glm(funding_dummy ~ average_score + I(average_score^2) + country + ., data = one_round, family = "binomial")

drops <- c("total_funding", "funding_dummy", "names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "venture_or_pe", "funding_type", "funding_rounds", "investors", "two_or_more")
seed_df<- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]
binomial3 <- glm(seed ~ average_score + I(average_score^2) + country + ., data = seed_df, family = "binomial")

drops <- c("total_funding", "funding_dummy", "names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "seed", "funding_type", "funding_rounds", "investors", "two_or_more")
venture_df<- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]
binomial4 <- glm(venture_or_pe ~ average_score + I(average_score^2) + country + ., data = venture_df, family = "binomial")

descriptions_categories$two_or_more <- ifelse(descriptions_categories$funding_rounds>1, 1, 0)
drops <- c("total_funding", "funding_dummy", "seed", "names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "venture_or_pe", "funding_type", "funding_rounds", "investors")
funded_only <- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]
binomial2 <- glm(two_or_more ~ average_score + I(average_score^2) + country + ., data = funded_only, family = "binomial")


stargazer(binomial1, binomial2, binomial3, binomial4, title="Specific Types of Funding Outcomes", dep.var.labels=c("", "", "", ""), dep.var.caption = "Funding Indicators", covariate.labels = c("mTurk Score", "mTurk Score*2", "Kenya", "Nigeria", "Uganda"), column.labels=c("First Funding Round", "Subsequent Funding Rounds", "Seed Funding", "Venture/PE Funding"), omit =c(7:224), no.space = T, digits = 2, omit.stat=c("aic"), font.size = "footnotesize")

## TOPICS
library("lda")
library("ggplot2")

corpus <- lexicalize(descriptions$clean_text)


alpha <- 1/22
eta <- 1/22

set.seed(110)
#set.seed(104)
#set.seed(103)
model <- lda.collapsed.gibbs.sampler(corpus$documents, 22, corpus$vocab, 1000, alpha = alpha, eta = eta, compute.log.likelihood=T, trace=0L)
topics <- top.topic.words(model$topics, n=7)
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


## FIGURE X: PRINT TOPIC WORDS
top.topic.words(model$topics, n=10)

## FIGURE X: RELATIONSHIP BETWEEN TOPICS AND MTURK SCORE

library(coefplot)
coefplot(lm1, sort="magnitude") + ylab("") + ggtitle("Coefficients from Regression of mTurk Score on Topics") + theme(plot.title = element_text(size = rel(1), hjust = 0))

## FIGURE X: TOPICS SORTED BY PREVALENCE
average_prop <- as.data.frame(colMeans(proportions))
average_prop$proportion <- average_prop$`colMeans(proportions)`
average_prop$`colMeans(proportions)` <- NULL
ggplot(average_prop, aes(x=reorder(rownames(average_prop), proportion), y=proportion)) + geom_bar(stat='identity') + coord_flip() + ylab("") + ggtitle("Topics by Prevalence") + theme(plot.title = element_text(size = rel(1), hjust = 0), axis.title.y=element_blank())

## TABLE X: REGRESSION OF FUNDING DUMMY ON SCORE/TOPICS INTERACTION
topic_proportions <- as.data.frame(counts/(rowSums(counts)))
# interaction between score and topics
score_topics <- data.frame(matrix(0, nrow=length(descriptions), ncol=length(topic_proportions)))
colnames(score_topics) <- paste("Score", colnames(topic_proportions), sep = "_")

for (i in 1:nrow(descriptions)) {
  for (j in 1:ncol(topic_proportions)) {
    score_topics[i,j] <- descriptions$average_score[i]*topic_proportions[i,j]
  }
}

##regress funding on score/topics 
score_topics_df <- cbind(topic_proportions, score_topics)
score_topics_df$country <- descriptions_categories$country 
score_topics_df$funding_dummy <- descriptions_categories$funding_dummy

topics_df <- topic_proportions
topics_df$funding_dummy <- descriptions_categories$funding_dummy
topics_df$country <- descriptions_categories$country
binomial1 <- glm(funding_dummy ~ . + country, topics_df, family="binomial")
binomial2 <- glm(funding_dummy ~ . + country, score_topics_df, family="binomial")

stargazer(binomial1, binomial2, title="Logistic Regression of Funding Dummy on Topic-Score Interaction", dep.var.labels="", 
    dep.var.caption = "Funding Indicator", covariate.labels = c("T1: quality", "T2: health", "T3: banking", "T4: gaming/networks",
    "T5: mobile platforms", "T6: energy", "T7: data", "T8: media", "T9: content", "T10: business solutions", "T11: entrepreneurship",
    "T12: education", "T13: farming", "T14: development/non-profit", "T15: events", "T16: time", "T17: service", "T18: delivery", 
    "T19: real estate", "T20: SMS", "T21: e-commerce", "T22: safaris", "Score x T1: quality", "Score x T2: health", "Score x T3: banking", "Score x T4: gaming/networks",
    "Score x T5: mobile platforms", "Score x T6: energy", "Score x T7: data", "Score x T8: media", "Score x T9: content", "Score x T10: business solutions", "Score x T11: entrepreneurship",
    "Score x T12: education", "Score x T13: farming", "Score x T14: development/non-profit", "Score x T15: events", "Score x T16: time", "Score x T17: service", "Score x T18: delivery", 
    "Score x T19: real estate", "Score x T20: SMS", "Score x T21: e-commerce", "Score x T22: safaris", "Kenya", "Nigeria", "Uganda"), 
    column.labels=c("Model 1", "Model 2"), no.space = T, digits = 2, omit.stat=c("aic"), font.size = "footnotesize", single.row = TRUE)


## TABLE X: REGRESSION OF FUNDING ON T3 and T3 INTERACTIONS

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
binomial1 <- glm(funding_dummy ~ . + country, topics_df, family="binomial")
binomial2 <- glm(funding_dummy ~ . + country, topics_interactions, family="binomial")


stargazer(binomial1, binomial2, title="Interactions with Banking Topic", dep.var.labels="",
          covariate.labels = c("T1: quality", "T2: health", "T3: banking", "T4: gaming/networks",
"T5: mobile platforms", "T6: energy", "T7: data", "T8: media", "T9: content", "T10: business solutions", "T11: entrepreneurship",
"T12: education", "T13: farming", "T14: development/non-profit", "T15: events", "T16: time", "T17: service", "T18: delivery",
"T19: real estate", "T20: SMS", "T21: e-commerce", "T22: safaris", "T3 x T1: quality", "T3 x T2: health", "T3 x T4: gaming/networks",
"T3 x T5: mobile platforms", "T3 x T6: energy", "T3 x T7: data", "T3 x T8: media", "T3 x T9: content", "T3 x T10: business solutions", "T3 x T11: entrepreneurship",
"T3 x T12: education", "T3 x T13: farming", "T3 x T14: development/non-profit", "T3 x T15: events", "T3 x T16: time", "T3 x T17: service", "T3 x T18: delivery",
"T3 x T19: real estate", "T3 x T20: SMS", "T3 x T21: e-commerce", "T3 x T22: safaris", "Kenya", "Nigeria", "Uganda"),
        dep.var.caption = "Funding Indicator", no.space = T, digits = 2, omit.stat=c("aic"), font.size = "footnotesize",
        notes=c("All individual topics and interactions with T3 are included.",
                "Only significant interactions are displayed."))
                 


## TABLE X: REGRESSION OF FUNDING ON T14 and T14 INTERACTIONS

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
binomial1 <- glm(funding_dummy ~ . + country, topics_df, family="binomial")
binomial2 <- glm(funding_dummy ~ . + country, topics_interactions, family="binomial")


stargazer(binomial1, binomial2, title="Interactions with Development/Non-Profit Topic", dep.var.labels="",
          covariate.labels = c("T1: quality", "T2: health", "T3: banking", "T4: gaming/networks",
                               "T5: mobile platforms", "T6: energy", "T7: data", "T8: media", "T9: content", "T10: business solutions", "T11: entrepreneurship",
                               "T12: education", "T13: farming", "T14: development/non-profit", "T15: events", "T16: time", "T17: service", "T18: delivery",
                               "T19: real estate", "T20: SMS", "T21: e-commerce", "T22: safaris", "T14 x T1: quality", "T14 x T2: health", "T14 x T3: banking", 
                               "T14 x T4: gaming/networks",
                               "T14 x T5: mobile platforms", "T14 x T6: energy", "T14 x T7: data", "T14 x T8: media", "T14 x T9: content", "T14 x T10: business solutions", "T14 x T11: entrepreneurship",
                               "T14 x T12: education", "T14 x T13: farming", "T14 x T15: events", "T14 x T16: time", "T14 x T17: service", "T14 x T18: delivery",
                               "T14 x T19: real estate", "T14 x T20: SMS", "T14 x T21: e-commerce", "T14 x T22: safaris", "Kenya", "Nigeria", "Uganda"),
          dep.var.caption = "Funding Indicator", no.space = T, digits = 2, omit.stat=c("aic"), font.size = "footnotesize",
          notes=c("All individual topics and interactions with T14 are included.",
                  "Only significant interactions are displayed."))


# APPENDIX TABLE X: REGRESSION OF FUNDING ON T8 and T8 INTERACTIONS

# topics_interactions <- topic_proportions
# topics_interactions$V8.V1 <- topics_interactions$V8*topics_interactions$V1
# topics_interactions$V8.V2 <- topics_interactions$V8*topics_interactions$V2
# topics_interactions$V8.V3 <- topics_interactions$V8*topics_interactions$V3
# topics_interactions$V8.V4 <- topics_interactions$V8*topics_interactions$V4
# topics_interactions$V8.V5 <- topics_interactions$V8*topics_interactions$V5
# topics_interactions$V8.V6 <- topics_interactions$V8*topics_interactions$V6
# topics_interactions$V8.V7 <- topics_interactions$V8*topics_interactions$V7
# topics_interactions$V8.V9 <- topics_interactions$V8*topics_interactions$V9
# topics_interactions$V8.V10 <- topics_interactions$V8*topics_interactions$V10
# topics_interactions$V8.V11 <- topics_interactions$V8*topics_interactions$V11
# topics_interactions$V8.V12 <- topics_interactions$V8*topics_interactions$V12
# topics_interactions$V8.V13 <- topics_interactions$V8*topics_interactions$V13
# topics_interactions$V8.V14 <- topics_interactions$V8*topics_interactions$V14
# topics_interactions$V8.V15 <- topics_interactions$V8*topics_interactions$V15
# topics_interactions$V8.V16 <- topics_interactions$V8*topics_interactions$V16
# topics_interactions$V8.V17 <- topics_interactions$V8*topics_interactions$V17
# topics_interactions$V8.V18 <- topics_interactions$V8*topics_interactions$V18
# topics_interactions$V8.V19 <- topics_interactions$V8*topics_interactions$V19
# topics_interactions$V8.V20 <- topics_interactions$V8*topics_interactions$V20
# topics_interactions$V8.V21 <- topics_interactions$V8*topics_interactions$V21
# topics_interactions$V8.V22 <- topics_interactions$V8*topics_interactions$V22
# 
# topics_interactions$funding_dummy <- descriptions_categories$funding_dummy
# topics_interactions$country <- descriptions_categories$country
# binomial1 <- glm(funding_dummy ~ . + country, topics_df, family="binomial")
# binomial2 <- glm(funding_dummy ~ ., topics_interactions, family="binomial")
# 
# stargazer(binomial1, binomial2, title="Interactions with Media Topic", dep.var.labels="",
#           covariate.labels = c("T1: quality", "T2: health", "T3: banking", "T4: gaming/networks",
#                                "T5: mobile platforms", "T6: energy", "T7: data", "T8: media", "T9: content", "T10: business solutions", "T11: entrepreneurship",
#                                "T12: education", "T13: farming", "T14: development/non-profit", "T15: events", "T16: time", "T17: service", "T18: delivery",
#                                "T19: real estate", "T20: SMS", "T21: e-commerce", "T22: safaris", "T8 x T1: quality", "T8 x T2: health", "T8 x T3: banking", 
#                                "T8 x T4: gaming/networks",
#                                "T8 x T5: mobile platforms", "T8 x T6: energy", "T8 x T7: data", "T8 x T8: media", "T8 x T9: content", "T8 x T10: business solutions", "T8 x T11: entrepreneurship",
#                                "T8 x T12: education", "T8 x T13: farming", "T8 x T15: events", "T8 x T16: time", "T8 x T17: service", "T8 x T18: delivery",
#                                "T8 x T19: real estate", "T8 x T20: SMS", "T8 x T21: e-commerce", "T8 x T22: safaris", "Kenya", "Nigeria", "Uganda"),
#           dep.var.caption = "Funding Indicator", no.space = T, digits = 2, omit.stat=c("aic", "ll"), font.size = "footnotesize",
#           notes=c("All individual topics and interactions with T8 are included.",
#                   "Only significant interactions are displayed."))



#################################################
# APPENDIX TABLES
#################################################


## APPENDIX TABLE X: RELATIONSHIP BETWEEN SCORE AND TOTAL FUNDING AMOUNTS
library(stargazer)
drops <- c("funding_dummy","names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "investors", "investor_type", "funding_type", "funding_rounds")
total_df <- descriptions_categories[ , !(names(descriptions_categories) %in% drops)]

under_10m <- filter(total_df, total_funding<10000000)
lm1 <- lm(total_funding ~ average_score + I(average_score^2) + country + ., under_10m)

under_1m <- filter(total_df, total_funding<1000000)
lm3 <- lm(total_funding ~ average_score + I(average_score^2) + country + ., under_1m)

under_400k <- filter(total_df, total_funding<400000)
lm4 <- lm(total_funding ~ average_score + I(average_score^2) + country + ., under_400k)

under_200k <- filter(total_df, total_funding<200000)
lm5 <- lm(total_funding ~ average_score + I(average_score^2) + country + ., under_200k)

under_50k <- filter(total_df, total_funding<50000)
lm6 <- lm(total_funding ~ average_score + I(average_score^2) + country + ., under_50k)

under_25k <- filter(total_df, total_funding<25000)
lm7 <- lm(total_funding ~ average_score + I(average_score^2) + country + ., under_25k)

stargazer(lm1, lm3, lm4, lm5, lm6, lm7, title="Regressions of Total Funding on mTurk Score", dep.var.labels="", dep.var.caption = "Funding Amount: Less than...", covariate.labels = c("mTurk Score", "mTurk Score*2", "Kenya", "Nigeria", "Uganda"), column.labels = c("\textdollar 10m", "\textdollar 1m", "\textdollar 400k", "\textdollar 200k", "\textdollar 50k", "\textdollar 25k"), omit =c(7:224), no.space = T, digits = 0, df=F, omit.stat=c("ll", "f", "ser", "adj.rsq"), font.size = "footnotesize")




# APPENDIX TABLE X: WORDS DISTINGUISHING LOW FROM MIDDLE SCORES
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
xtable(df, format="latex", include.rownames = FALSE)

coef_table <- xtable(df, format="latex")
print(coef_table, include.rownames = FALSE, size="small")

# APPENDIX TABLE X: WORDS MIDDLE FROM HIGH SCORES
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
xtable(df, format="latex", include.rownames = FALSE)

coef_table <- xtable(df, format="latex")
print(coef_table, include.rownames = FALSE, size="small")



#APPENDIX FIGURE X: HARMONIC MEAN METHOD FOR SELECTING OPTIMUM TOPIC NUMBER

library(topicmodels)

harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

# The log-likelihood values are then determined by first fitting the model using for example
k = 20
burnin = 1000
iter = 1000
keep = 50

text_dfm <- dfm(descriptions$clean_text)
# generate numerous topic models with different numbers of topics
number_of_topics <- seq(2, 50, 1) # in this case a sequence of numbers from 1 to 50, by ones.
set.seed(115)
fitted_many <- lapply(number_of_topics, function(k) LDA(text_dfm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
harmonic_mean <- sapply(logLiks_many, function(h) harmonicMean(h))

# inspect
plot(number_of_topics, harmonic_mean, type = "l")

# compute optimum number of topics
number_of_topics[which.max(harmonic_mean)]


## GENERATE LDAVIS
library(LDAvis)
library(servr)

theta <- t(apply(model$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(model$topics) + eta, 2, function(x) x/sum(x)))
doc.length <- sapply(corpus$documents, function(x) sum(x[2, ]))
vocab <- corpus$vocab
term.table <- word.counts(corpus$documents, vocab=NULL)
term.table <- sort(term.table, decreasing = TRUE)
term.frequency <- as.integer(term.table) 

StartupTopics <- list(phi = phi,
                      theta = theta,
                      doc.length = doc.length,
                      vocab = vocab,
                      term.frequency = term.frequency)

json <- createJSON(phi = StartupTopics$phi, 
                   theta = StartupTopics$theta, 
                   doc.length = StartupTopics$doc.length, 
                   vocab = StartupTopics$vocab, 
                   term.frequency = StartupTopics$term.frequency)

serVis(json, out.dir = 'vis2', open.browser = FALSE)

######################
# OUTSHEET FOR BROWSER
######################

#metadata
metadata <- descriptions
metadata$dup1 <- NULL
metadata$dup2 <- NULL
metadata$match <- NULL
metadata$score_sd <- NULL
metadata$clean_text <- NULL
metadata$num_responses <- NULL

setwd("/Users/nataliecarlson/Desktop/Summer16/SSA_Model_Browser/data_from_R")

con<-file('metadata.csv',encoding="UTF-8")
write.csv(metadata,file=con)

#tw.json
tw_object <- list()
words_df <- as.data.frame(t(model$topics))
for (i in 1:ncol(words_df)) {
  sorted_words <- words_df[order(-words_df[,i]),]
  tw_object <- append(tw_object, list(words=rownames(sorted_words[1:30,])))
  tw_object <- append(tw_object, list(weights=sorted_words[1:30, i]))
}

tw_object

#dt.json.zip
i <- list()
p <- list(0)
x <- list()
document_sums <- as.data.frame(t(model$document_sums))
for (j in 1:ncol(document_sums)) {
  docs <- which(document_sums[,j] != 0)
  docs_zeroindex <- docs - 1
  length <- length(docs)
  weights <- document_sums[docs, j]
  i <- append(i, docs_zeroindex)
  length <- p[[length(p)]] + length 
  p <- append(p, length)
  x <- append(x, weights)
}
i <- unlist(i)
p <- unlist(p)
x <- unlist(x)

write(i, "dt_i.csv", sep="\n")
write(p, "dt_p.csv", sep="\n")
write(x, "dt_x.csv", sep="\n")

