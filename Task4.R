user_cat=data_users %>% mutate(
  member_since=as.Date(member_since),
  Before_2020=if_else(
    member_since < as.Date("2020-01-01"), 
    TRUE, 
    FALSE
  )
) %>% filter(!is.na(Before_2020)) #Filter NA data
head(user_cat)
head(data_rv) #First 10 rows of data_rv

join_table=data_rv %>% 
  #Inner join by user_id
  inner_join(user_cat,by="user_id") %>%
  #Create new column for review length
  mutate(review_length=nchar(text)) %>%
  #Filter out NA value
  filter(!is.na(stars),!is.na(review_length))
review_by_group=join_table %>% 
  #Group by TRUE/False value
  group_by(Before_2020) %>% 
  #Summary
  summarise(
    #Mean
    average_stars_rating=mean(stars,na.rm=TRUE),
    #Average length of reviews
    avg_review_length = mean(review_length, na.rm = TRUE),    
    #Number of reviews
    n_reviews = n(),
    #Number of valid users
    n_unique_users = n_distinct(user_id))

head(review_by_group)
#Rename column names for the dataset
colnames(review_by_group)=c("Join before 2020","Average star rating","Average length of review ", "Number of reviews","Number of users")
#Draw table for the analysis
review_by_group %>% kbl(caption = "Summary by Group") %>%  kable_paper("hover",full_width=F,html_font = "Camberia")
#Draw boxplot graph for analysis and with x, and y axis
ggplot(join_table, aes(x = Before_2020, y = review_length)) +
  #Choose type of graph and colors
  geom_boxplot(fill = "white", alpha = 0.7) +
  #Choose statistic for x value 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  #Set title and x, and y labels
  labs(title = "Review Length Distribution by User Join Period",
       x = "User Group",
       y = "Review Length (characters)") +
  theme_minimal() +
  
  #Adjust title
  theme(plot.title = element_text(hjust = 0.5))