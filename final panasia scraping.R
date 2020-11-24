library(tidyverse)
library(rvest)
library(xml2)

url<-read_html("https://www.tripadvisor.in/Restaurant_Review-g304556-d4271075-Reviews-Pan_Asian-Chennai_Madras_Chennai_District_Tamil_Nadu.html")

npages<-url%>%
html_nodes("#taplc_location_reviews_list_resp_rr_resp_0 .pageNum")%>%
html_attr(name="data-page-number")%>%
tail(.,1)%>%
as.numeric()
npages


a<-0:(npages-1)
a
b<-10
b
res<-numeric(length = length(a))
res
for (i in seq_along(a)) {
res[i]<-a[i]*b
}

tableout<-data.frame()


for (i in res) {
cat(".")
 url_reviews <- paste0("https://www.tripadvisor.in/Restaurant_Review-g304556-d4271075-Reviews-or",i,
                        "-Pan_Asian-Chennai_Madras_Chennai_District_Tamil_Nadu.html",sep="")
  
  doc <- read_html(url_reviews) # Assign results to `doc`
  
  # Review Title
  doc %>% 
    html_nodes(".noQuotes") %>%
    html_text() -> review_title
  
  
  # Number of stars in review
  doc %>%
    html_nodes("#taplc_location_reviews_list_resp_rr_resp_0 .ui_bubble_rating") %>%
    html_attrs()%>%
gsub("ui_bubble_rating bubble_","", .)%>%
as.integer() / 10 -> review_star


temp.tableout<-data.frame(review_title,review_star)
tableout<-rbind(tableout,temp.tableout)
}

View(tableout)
write.csv(tableout,"C://Users//Sheeja Ayoob//Documents//panasia.csv")
