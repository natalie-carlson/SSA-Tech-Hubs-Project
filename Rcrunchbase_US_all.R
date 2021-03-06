#!/apps/R-3.2.3/bin/Rscript
#install.packages("rcrunchbase")
library(rcrunchbase)
library(dplyr)
library(methods)

Sys.setenv(CRUNCHBASE_KEY = "816189d97ed23a3e25188564185a5dfb")

# READ IN DATA
all_US_companies <- crunchbase_get_collection("organizations", locations="United States")
all_US_details <- crunchbase_get_collection("organizations", locations="United States") %>% crunchbase_get_details()

US_company_details <- list()
US_total_funding <- list()
US_names <- list()
US_categories <- list()
US_funding_rounds <- list()
US_funding_type <- list()

# GRAB RELEVANT DETAILS
for(i in 1:length(all_US_details)){
  if (is(try(all_US_details[[i]]$properties$description), "try-error")) {
    US_company_details[i] <- all_US_companies[i,]$properties.short_description
  }
  else if (is.null(all_US_details[[i]]$properties$description)) {
    US_company_details[i] <- all_US_companies[i,]$properties.short_description
  }
  else {
    try(US_company_details[i] <- all_US_details[[i]]$properties$description)
  }
  try(US_total_funding[i] <- all_US_details[[i]]$properties$total_funding_usd)
  US_names[i] <- all_US_companies[i,]$properties.name
  try(US_categories[i] <- paste(all_US_details[[i]]$relationships$categories$items$properties.name, collapse = "|"))
  try(US_funding_rounds[i] <- all_US_details[[i]]$relationships$funding_rounds$paging$total_items)
  try(US_funding_type[i] <- paste(all_US_details[[i]]$relationships$funding_rounds$items$properties.funding_type, collapse = "|"))
}

company_details <- US_company_details
total_funding <- US_total_funding
names <- US_names
categories <- US_categories
funding_rounds <- US_funding_rounds
funding_type <- US_funding_type

# CLEAN UP AND OUTSHEET CATEGORIES
categories_table <- unlist(categories)
write.csv(table(categories_table), "test_categories.csv")

#creating matrix
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

write.csv(category_matrix, "test_category_matrix.csv")

# CLEAN AND OUTSHEET OTHER DATA
descriptions <- gsub("[\r\n]", " ", company_details)
descriptions <- gsub("^ ", "", descriptions)
descriptions <- gsub(" $", "", descriptions)
descriptions.df <- as.data.frame(descriptions)
descriptions.df$total_funding <- total_funding
descriptions.df$names <- names
descriptions.df$funding_rounds <- funding_rounds
descriptions.df$funding_type <- funding_type
descriptions.df$categories <- US_categories
#descriptions.df$wordcount <- word_count(descriptions.df$descriptions)
#descriptions.df$wordcount <- as.numeric(descriptions.df$wordcount)
#descriptions.df <- filter(descriptions.df, wordcount>5)
descriptions.df <- data.frame(lapply(descriptions.df, as.character), stringsAsFactors=FALSE)
con<-file('US_descriptions.csv',encoding="UTF-8")
con2<-file('US_descriptions_only.csv',encoding="UTF-8")
write.csv(descriptions.df, file=con)
write.csv(descriptions.df$descriptions, file=con2, row.names = FALSE, quote = FALSE)