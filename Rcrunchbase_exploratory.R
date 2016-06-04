library(rcrunchbase)
library(magrittr)


#african_companies <- crunchbase_get_collection("organizations", locations="africa")

## Pull Nigeria Companies ##

nigerian_companies <- crunchbase_get_collection("organizations", locations="nigeria")

nigerian_companies_details <- crunchbase_get_collection("organizations", locations="nigeria") %>% crunchbase_get_details()

NG_company_details <- list()

for(i in 1:length(nigerian_companies_details)){
  if (is.null(nigerian_companies_details[[i]]$properties$description)) {
    NG_company_details[i] <- nigerian_companies[i,]$properties.short_description
  }
  else {
    NG_company_details[i] <- nigerian_companies_details[[i]]$properties$description
  }
}

## Pull Kenya Companies ##

kenyan_companies <- crunchbase_get_collection("organizations", locations="kenya")

kenyan_companies_details <- crunchbase_get_collection("organizations", locations="kenya") %>% crunchbase_get_details()

KE_company_details <- list()

for(i in 1:length(kenyan_companies_details)){
  if (is.null(kenyan_companies_details[[i]]$properties$description)) {
    KE_company_details[i] <- kenyan_companies[i,]$properties.short_description
  }
  else {
    KE_company_details[i] <- kenyan_companies_details[[i]]$properties$description
  }
}


library("tm")

## Clean Data ##

# lowercase
NG_company_details <- tolower(NG_company_details)
KE_company_details <- tolower(KE_company_details)

NG_company_details <- gsub("[\r\n]", " ", NG_company_details)
KE_company_details <- gsub("[\r\n]", " ", KE_company_details)

# Replace @UserName
NG_company_details <- gsub("@\\w+", " ", NG_company_details)
KE_company_details <- gsub("@\\w+", " ", KE_company_details)

# Remove punctuation
NG_company_details <- gsub("[[:punct:]]+", " ", NG_company_details)
KE_company_details <- gsub("[[:punct:]]+", " ", KE_company_details)

# Remove digits
NG_company_details <- gsub("[[:digit:]]+", " ", NG_company_details)
KE_company_details <- gsub("[[:digit:]]+", " ", KE_company_details)

# Remove links
NG_company_details <- gsub("http\\w+", " ", NG_company_details)
KE_company_details <- gsub("http\\w+", " ", KE_company_details)

myStopwords <- c(stopwords("en"), "e", "s", "m", "d", "t")

# Remove Stopwords
NG_company_details <- removeWords(NG_company_details, myStopwords)
KE_company_details <- removeWords(KE_company_details, myStopwords)

# Remove blank spaces at the beginning
NG_company_details <- gsub("^ ", "", NG_company_details)
KE_company_details <- gsub("^ ", "", KE_company_details)

# Remove blank spaces at the end
NG_company_details <- gsub(" $", "", NG_company_details)
KE_company_details <- gsub(" $", "", KE_company_details)

# Replace blank space (“rt”)
NG_company_details <- gsub("rt", "", NG_company_details)
KE_company_details <- gsub("rt", "", KE_company_details)

# Remove tabs
NG_company_details <- gsub("[ |\t]{2,}", " ", NG_company_details)
KE_company_details <- gsub("[ |\t]{2,}", " ", KE_company_details)

## Word Clouds

library("wordcloud")

#NG_company_corpus <- Corpus(VectorSource(NG_company_details))
#KE_company_corpus <- Corpus(VectorSource(KE_company_details))

#wordcloud(NG_company_corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
#wordcloud(KE_company_corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)

## Topics

library("lda")
library("ggplot2")

both_company_details <- append(NG_company_details, KE_company_details)

both_corpus <-lexicalize(both_company_details)
NG_corpus <- lexicalize(NG_company_details)
KE_corpus <- lexicalize(KE_company_details)

both_model <- lda.collapsed.gibbs.sampler(both_corpus$documents, 15, both_corpus$vocab, 300, 1/15, 1/15, compute.log.likelihood=T, trace=0L)
topics <- top.topic.words(both_model$topics, n=10)
topics

qplot(1:300, both_model$log.likelihood[1,])
