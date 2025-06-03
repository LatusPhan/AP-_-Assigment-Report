library(tidyverse)
library(knitr)
library(kableExtra)
data_users=read.csv("users.csv")
data_review=read_csv("reviews.csv")
head(data_users)
head(data_review)
data_users<- data_users %>% select(-review_count, -average_stars)
head(data_users)
#Create new dataframe from data of data_users
data_users <- data_users %>%
  
  #Create a new column for data_users for Veteran, Intermediate, and New by mutate
  mutate( 
    
    #The column is defined as users_group
    users_group = case_when(
      
      #Case_when to categorise based on member_since of data_users
      
      #Case 1: Before 2017-01-01
      data_users$member_since < as.Date("2017-01-01") ~ "Veteran",
      
      #Case 2: Before 2022-12-31 and after 2017-01-01
      data_users$member_since >= as.Date("2017-01-01") &data_users$member_since <= as.Date("2022-12-31") ~ "Intermediate",
      
      #Case 3: After 2022-12-31
      data_users$member_since > as.Date("2022-12-31") ~ "New",
      #If data can not be categorised,assign NA 
      TRUE ~ NA_character_
    )
  )
joined_data= data_review %>% left_join(data_users, by="user_id")
head(joined_data)
user_stats <- joined_data %>%group_by(users_group) %>% 
  #Use summarise to reduce data of data_users into 1 row then return neeeded statistics as defined names like total_users,avg_review_stars...
  summarise(
    # Total number of users in the first collumn
    total_users = n_distinct(user_id),
    
    # Average review stars per user in the second collumn
    avg_review_stars = mean(stars, na.rm = TRUE),
    
    # Average number of reviews per user in the third collumn
    avg_reviews_per_user = mean(n_distinct(review_id), na.rm = TRUE)
  )
#Change the name of columns for easy-reading

colnames(user_stats)=c("Total Users","Average review stars per user","Average number of reviews per user")
user_stats %>% 
  #Set the title 
  kbl(caption = "Table for numbers of users and their reviews")%>%
  
  #Format the table, hover indicate the change of color in html, 
  #Full_width=F expanding across paper, 
  #Hover using to change color in html/ webpage
  
  kable_classic("hover",full_width=F,html_font = "Camberia")

user_stats

