library(rcrunchbase)
library(magrittr)
library(quanteda)
library(qdap)
library("tm")

## Pull U.S. Companies by Category to Construct Custom Sample ##

mobile_payments_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Mobile, Mobile Payments")
mobile_payments_details <- crunchbase_get_collection("organizations", locations="United States", categories="Mobile, Mobile Payments") %>% crunchbase_get_details()

mobile_apps_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Mobile, Apps, Software")
mobile_apps_details <- crunchbase_get_collection("organizations", locations="United States", categories="Mobile, Apps, Software") %>% crunchbase_get_details()

ecommerce_companies <- crunchbase_get_collection("organizations", locations="United States", categories="E-Commerce, Shopping")
ecommerce_details <- crunchbase_get_collection("organizations", locations="United States", categories="E-Commerce, Shopping")  %>% crunchbase_get_details()

education_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Education, Mobile")
education_details <- crunchbase_get_collection("organizations", locations="United States", categories="Education, Mobile")  %>% crunchbase_get_details()

internet_apps_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Internet, Apps")
internet_apps_details <- crunchbase_get_collection("organizations", locations="United States", categories="Internet, Apps") %>% crunchbase_get_details()

software_IT_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Software, Information Technology, Mobile")
software_IT_details <- crunchbase_get_collection("organizations", locations="United States", categories="Software, Information Technology, Mobile") %>% crunchbase_get_details()

health_IT_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Health Care, Information Technology, mHealth")
health_IT_details <- crunchbase_get_collection("organizations", locations="United States", categories="Health Care, Information Technology, mHealth") %>% crunchbase_get_details()

agri_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Agriculture")
agri_details <- crunchbase_get_collection("organizations", locations="United States", categories="Agriculture") %>% crunchbase_get_details()

energy_software_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Energy, Enterprise Software")
energy_software_details <- crunchbase_get_collection("organizations", locations="United States", categories="Energy, Enterprise Software") %>% crunchbase_get_details()

social_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Social Entrepreneurship")
social_details <- crunchbase_get_collection("organizations", locations="United States", categories="Social Entrepreneurship") %>% crunchbase_get_details()

financial_software_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Financial Services, Software")
financial_software_details <- crunchbase_get_collection("organizations", locations="United States", categories="Financial Services, Software")  %>% crunchbase_get_details()

financial_banking_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Financial Services, Banking") 
financial_banking_details <- crunchbase_get_collection("organizations", locations="United States", categories="Financial Services, Banking") %>% crunchbase_get_details()

nonprofit_communities_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Non Profit, Communities") 
nonprofit_communities_details <- crunchbase_get_collection("organizations", locations="United States", categories="Non Profit, Communities") %>% crunchbase_get_details()

consulting_mobile_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Consulting, Mobile")
consulting_mobile_details <- crunchbase_get_collection("organizations", locations="United States", categories="Consulting, Mobile") %>% crunchbase_get_details()

consulting_advertising_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Consulting, Advertising")
consulting_advertising_details <- crunchbase_get_collection("organizations", locations="United States", categories="Consulting, Advertising")  %>% crunchbase_get_details()

socialmedia_communities_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Social Media, Communities")
socialmedia_communities_details <- crunchbase_get_collection("organizations", locations="United States", categories="Social Media, Communities") %>% crunchbase_get_details()

curatedweb_news_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Curated Web, News")
curatedweb_news_details <- crunchbase_get_collection("organizations", locations="United States", categories="Curated Web, News") %>% crunchbase_get_details()

curatedweb_ecommerce_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Curated Web, E-Commerce")
curatedweb_ecommerce_details <- crunchbase_get_collection("organizations", locations="United States", categories="Curated Web, E-Commerce") %>% crunchbase_get_details()

nonprofit_education_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Non Profit, Education") 
nonprofit_education_details <- crunchbase_get_collection("organizations", locations="United States", categories="Non Profit, Education") %>% crunchbase_get_details()

ecommerce_software_companies <- crunchbase_get_collection("organizations", locations="United States", categories="E-Commerce, Software, Internet")
ecommerce_software_details <- crunchbase_get_collection("organizations", locations="United States", categories="E-Commerce, Software, Internet")  %>% crunchbase_get_details()

education_social_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Education, Social Media")
education_social_details <- crunchbase_get_collection("organizations", locations="United States", categories="Education, Social Media")  %>% crunchbase_get_details()

ecommerce_retail_companies <- crunchbase_get_collection("organizations", locations="United States", categories="E-Commerce, Retail, Internet")
ecommerce_retail_details <- crunchbase_get_collection("organizations", locations="United States", categories="E-Commerce, Retail, Internet")  %>% crunchbase_get_details()

advertising_social_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Advertising, Social Media, Publishing")
advertising_social_details <- crunchbase_get_collection("organizations", locations="United States", categories="E-Commerce, Retail, Internet")  %>% crunchbase_get_details()

energy_IT_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Energy, Information Technology")
energy_IT_details <- crunchbase_get_collection("organizations", locations="United States", categories="Energy, Information Technology")  %>% crunchbase_get_details()

travel_search_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Travel, Search")
travel_search_details <- crunchbase_get_collection("organizations", locations="United States", categories="Travel, Search")  %>% crunchbase_get_details()

realestate_companies <- crunchbase_get_collection("organizations", locations="United States", categories="Real Estate")
travel_search_details <- crunchbase_get_collection("organizations", locations="United States", categories="Travel, Search")  %>% crunchbase_get_details()
