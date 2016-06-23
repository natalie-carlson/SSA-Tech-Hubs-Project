
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
df <- df[order(df$coefficient),]
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
stargazer(binomial1, binomial2, binomial3, title="Logistic Regression of Funding Dummy on mTurk Score", dep.var.labels="", dep.var.caption = "Funding indicator", covariate.labels = c("mTurk Score", "mTurk Score*2", "Kenya", "Nigeria", "Uganda", "Software", "Mobile", "E-Commerce", "Education"), column.labels=c("Model 1", "Model 2", "Model 3"), omit =c(11:224), no.space = T, digits = 2, omit.stat=c("aic", "ll"))

## TABLE X: RELATIONSHIP BETWEEN SCORE AND TOTAL FUNDING AMOUNTS
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
funded_only <- filter(descriptions_categories, funding_dummy==1)
drops <- c("total_funding", "funding_dummy", "seed", "names", "descriptions", "wordcount", "score_sd", "num_responses", "clean_text", "dup1", "dup2", "match", "venture_or_pe", "funding_type", "funding_rounds", "investors")
funded_only <- funded_only[ , !(names(funded_only) %in% drops)]
binomial2 <- glm(two_or_more ~ average_score + I(average_score^2) + country, data = funded_only, family = "binomial")


stargazer(binomial1, binomial2, binomial3, binomial4, title="Specific Types of Funding Outcomes", dep.var.labels=c("", "", "", ""), dep.var.caption = "Funding Indicators", covariate.labels = c("mTurk Score", "mTurk Score*2", "Kenya", "Nigeria", "Uganda"), column.labels=c("First Funding Round", "Subsequent Funding Rounds", "Seed Funding", "Venture/PE Funding"), omit =c(7:224), no.space = T, digits = 2, omit.stat=c("aic", "ll"), font.size = "footnotesize")





#################################################
# APPENDIX TABLES
#################################################

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

