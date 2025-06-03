data_users=read.csv("users.csv")
data_review=read_csv("reviews.csv")
data_bs= read.csv("businesses.csv")
library(ggplot2)

head(data_bs)

# Create new dataframe for summary statistics
business_stats <- data_bs %>% group_by(state) %>% 
  summarise(
    # Total number of businesses
    total_business = n_distinct(business_id),
    
    # Average review stars per business
    avg_review_stars = mean(business.avg.stars, na.rm = TRUE),
    
    # Number of reviews of all businesses
    num_reviews = sum(review_count, na.rm = TRUE)
  )

head(business_stats)
joined_data= data_review %>% left_join(data_bs, by="business_id")
head(joined_data)

# Add user count from a separate data_users dataframe
stat_join_table <- joined_data %>%
  summarise(
    user_count = n_distinct(user_id),
    averge_count= mean(stars,na.rm = TRUE),
    number_of_review=n_distinct(review_id)
    )

# Rename columns
colnames(stat_join_table) <- c(
  "Total number of unique users",
  "Average review stars ",
  "Total number of reviews"
)
# Create formatted table
stat_join_table %>%
  kbl(caption = "Table: Summary of Total and Average  Review Stars, and Total Reviews") %>%
  kable_classic("hover", full_width = F, html_font = "Cambria")

stat_join_table


#Plot the data

#Load dataset and choose x,y variables
plot_state =ggplot(business_stats, aes(x = state, y = avg_review_stars,size = avg_review_stars))+
  #Use scaterplot for this visualization with 0.5 in transparency
  geom_point(alpha=0.5)+
  #Add title and labels for x and y
  labs(
    title = "Average Review Ratings by State",
    x = "State",
    y = "Average Review Rating",
    color = "Avg Rating",
    size = "Total Reviews"
  ) +
  #Select theme
  theme_minimal()+ 
  #Adjust labels
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
data_users=read.csv("users.csv")
data_review=read_csv("reviews.csv")
data_bs= read.csv("businesses.csv")
head(data_bs)
# Create new dataframe for summary statistics
business_stats <- data_bs %>% group_by(state) %>% 
  summarise(
    # Total number of businesses
    total_business = n_distinct(business_id),
    
    # Average review stars per business
    avg_review_stars = mean(business.avg.stars, na.rm = TRUE),
    
    # Number of reviews of all businesses
    num_reviews = sum(review_count, na.rm = TRUE)
  )

head(business_stats)
joined_data= data_review %>% left_join(data_bs, by="business_id")
head(joined_data)

# Add user count from a separate data_users dataframe
stat_join_table <- joined_data %>%
  summarise(
    user_count = n_distinct(user_id),
    averge_count= mean(stars,na.rm = TRUE),
    number_of_review=n_distinct(review_id)
  )

# Rename columns
colnames(stat_join_table) <- c(
  "Total number of unique users",
  "Average review stars ",
  "Total number of reviews"
)
# Create formatted table
stat_join_table %>%
  kbl(caption = "Table: Summary of Total and Average  Review Stars, and Total Reviews") %>%
  kable_classic("hover", full_width = F, html_font = "Cambria")

stat_join_table


#Plot the data

#Load dataset and choose x,y variables
plot_state =ggplot(business_stats, aes(x = state, y = avg_review_stars,size = avg_review_stars))+
  #Use scaterplot for this visualization with 0.5 in transparency
  geom_point(alpha=0.5)+
  #Add title and labels for x and y
  labs(
    title = "Average Review Ratings by State",
    x = "State",
    y = "Average Review Rating",
    color = "Avg Rating",
    size = "Total Reviews"
  ) +
  #Select theme
  theme_minimal()+ 
  #Adjust labels
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
print(plot_state)
