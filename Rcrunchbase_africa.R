#!/apps/R/lib64/R/bin/Rscript

library(rcrunchbase)
library(magrittr)
library(quanteda)
library(qdap)
library("tm")

## Pull African Companies ##

ugandan_companies <- crunchbase_get_collection("organizations", locations="uganda")
ugandan_companies_details <- crunchbase_get_collection("organizations", locations="uganda") %>% crunchbase_get_details()

nigerian_companies <- crunchbase_get_collection("organizations", locations="nigeria")
nigerian_companies_details <- crunchbase_get_collection("organizations", locations="nigeria") %>% crunchbase_get_details()

kenyan_companies <- crunchbase_get_collection("organizations", locations="kenya")
kenyan_companies_details <- crunchbase_get_collection("organizations", locations="kenya") %>% crunchbase_get_details()

ghanaian_companies <- crunchbase_get_collection("organizations", locations="ghana")
ghanaian_companies_details <- crunchbase_get_collection("organizations", locations="ghana") %>% crunchbase_get_details()

#Create text lists

NG_company_details <- list()
NG_total_funding <- list()
NG_names <- list()
NG_categories <- list()

for(i in 1:length(nigerian_companies_details)){
  if (is.null(nigerian_companies_details[[i]]$properties$description)) {
    NG_company_details[i] <- nigerian_companies[i,]$properties.short_description
  }
  else {
    NG_company_details[i] <- nigerian_companies_details[[i]]$properties$description
  }
  NG_total_funding[i] <- nigerian_companies_details[[i]]$properties$total_funding_usd
  NG_names[i] <- nigerian_companies[i,]$properties.name
  NG_categories[i] <- paste(nigerian_companies_details[[i]]$relationships$categories$items$properties.name, collapse = "|")
}

KE_company_details <- list()
KE_total_funding <- list()
KE_names <- list()
KE_categories <- list()

for(i in 1:length(kenyan_companies_details)){
  if (is.null(kenyan_companies_details[[i]]$properties$description)) {
    KE_company_details[i] <- kenyan_companies[i,]$properties.short_description
  }
  else {
    KE_company_details[i] <- kenyan_companies_details[[i]]$properties$description
  }
  KE_total_funding[i] <- kenyan_companies_details[[i]]$properties$total_funding_usd
  KE_names[i] <- kenyan_companies[i,]$properties.name
  KE_categories[i] <- paste(kenyan_companies_details[[i]]$relationships$categories$items$properties.name, collapse = "|")
}

GH_company_details <- list()
GH_total_funding <- list()
GH_names <- list()
GH_categories <- list()

for(i in 1:length(ghanaian_companies_details)){
  if (is.null(ghanaian_companies_details[[i]]$properties$description)) {
    GH_company_details[i] <- ghanaian_companies[i,]$properties.short_description
  }
  else {
    GH_company_details[i] <- ghanaian_companies_details[[i]]$properties$description
  }
  GH_total_funding[i] <- ghanaian_companies_details[[i]]$properties$total_funding_usd
  GH_names[i] <- ghanaian_companies[i,]$properties.name
  GH_categories[i] <- paste(ghanaian_companies_details[[i]]$relationships$categories$items$properties.name, collapse = "|")
}

UG_company_details <- list()
UG_total_funding <- list()
UG_names <- list()
UG_categories <- list()

for(i in 1:length(ugandan_companies_details)){
  if (is.null(ugandan_companies_details[[i]]$properties$description)) {
    UG_company_details[i] <- ugandan_companies[i,]$properties.short_description
  }
  else {
    UG_company_details[i] <- ugandan_companies_details[[i]]$properties$description
  }
  UG_total_funding[i] <- ugandan_companies_details[[i]]$properties$total_funding_usd
  UG_names[i] <- ugandan_companies[i,]$properties.name
  UG_categories[i] <- paste(ugandan_companies_details[[i]]$relationships$categories$items$properties.name, collapse = "|")
}

ghanaian_companies$country <- "Ghana"
kenyan_companies$country <- "Kenya"
nigerian_companies$country <- "Nigeria"
ugandan_companies$country <- "Uganda"

country <- append(nigerian_companies$country, kenyan_companies$country)
country <- append(country, ghanaian_companies$country)
country <- append(country, ugandan_companies$country)

company_details <- append(NG_company_details, KE_company_details)
company_details <- append(company_details, GH_company_details)
company_details <- append(company_details, UG_company_details)

total_funding <- append(NG_total_funding, KE_total_funding)
total_funding <- append(total_funding, GH_total_funding)
total_funding <- append(total_funding, UG_total_funding)

names <- append(NG_names, KE_names)
names <- append(names, GH_names)
names <- append(names, UG_names)

categories <- append(NG_categories, KE_categories)
categories <- append(categories, GH_categories)
categories <- append(categories, UG_categories)

#categories_table <- unlist(categories)
#write.csv(table(categories_table), "categories.csv")

#cleaning up categories and creating matrix
categories <- lapply(categories, function(x) strsplit(x, "\\|"))
all_combinations <- unique(categories)
all_categories <- unique(unlist(categories))
category_matrix <- data.frame(matrix(0, nrow=length(categories), ncol=length(all_categories)))
colnames(category_matrix) <- all_categories

for (i in 1:nrow(category_matrix)) {
  for (j in 1:length(all_categories)){
    if (all_categories[j] %in% categories[[i]][[1]]) category_matrix[i,j] <- 1 
  }
}

category_matrix$names <- unlist(names)
category_matrix$descriptions <- unlist(company_details)
category_matrix$country <- country
write.csv(category_matrix, "category_matrix.csv")


#outsheet for survey
# descriptions <- gsub("[\r\n]", " ", company_details)
# descriptions <- gsub("^ ", "", descriptions)
# descriptions <- gsub(" $", "", descriptions)
# descriptions.df <- as.data.frame(descriptions)
# descriptions.df$total_funding <- total_funding
# descriptions.df$names <- names
# descriptions.df$wordcount <- word_count(descriptions.df$descriptions)
# library(dplyr)
# descriptions.df <- filter(descriptions.df, wordcount>5)
# descriptions.df <- data.frame(lapply(descriptions.df, as.character), stringsAsFactors=FALSE)
# con<-file('descriptions.csv',encoding="UTF-8")
# con2<-file('descriptions_only.csv',encoding="UTF-8")
# write.csv(descriptions.df, file=con)
# write.csv(descriptions.df$descriptions, file=con2, row.names = FALSE, quote = FALSE)

## Clean Data ##

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

## Word Clouds

library("wordcloud")

#company_corpus <- Corpus(VectorSource(company_details))

#wordcloud(company_corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)


## Topics

library("lda")
library("ggplot2")

corpus <- lexicalize(company_details)

  
alpha <- 1/20
eta <- 1/20

set.seed(115)
model <- lda.collapsed.gibbs.sampler(corpus$documents, 20, corpus$vocab, 1000, alpha = alpha, eta = eta, compute.log.likelihood=T, trace=0L)
topics <- top.topic.words(model$topics, n=7)
topics

#qplot(1:300, model$log.likelihood[1,])

# LDA Visualization
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

serVis(json, out.dir = 'vis2', open.browser = TRUE)