data_users=read.csv("users.csv")
data_review=read_csv("reviews.csv")
head(data_users)
head(data_review)
joined_data= data_review %>% left_join(data_users, by="user_id")
head(joined_data)
head(data_users)
top_10= joined_data %>% group_by(user_id) %>%summarise(
  review_count_number= n_distinct(review_id)
) %>% arrange(desc(review_count_number))
top_10=top_10 %>% left_join(data_users,by="user_id")%>% 
  slice(2:11)
head(top_10)
plot_3=ggplot(data=top_10, aes(x = user_id, y = review_count_number)) +
  geom_bar(stat = "identity", fill = "black", color = "black") + 
  labs(
    title = "Rating Distribution of Top 10 Most Active Users",
    x = "User ID",
    y = "Review Stars"
  ) +
  theme_minimal() 
print(plot_3)
