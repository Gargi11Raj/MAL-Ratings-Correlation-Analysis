library(tidyverse)# Data manipulation and visualization
library(scales)# Converting data values to units
library(patchwork) # For Patching multiple ggplots together

tidy_anime <- read_csv(file="https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
View(tidy_anime)

tidy_anime <- tidy_anime%>%
  select(-c(title_synonyms,synopsis,background,related,genre,studio,producers))

unique_anime <- tidy_anime%>%
  distinct()%>%
  filter(!is.na(title_english))
View(unique_anime)

tv_anime <- unique_anime%>%filter(type=="TV")
tv_plot <-tv_anime%>%
  mutate(graph_name=paste0(title_english," (",premiered,")"))%>%
  top_n(-20,wt=popularity) %>% 
  ggplot(aes(reorder(graph_name,desc(popularity)),popularity,colour=title_english))+
  geom_point(show.legend = FALSE)+
  geom_segment(aes(x=graph_name,xend=graph_name,y=0,yend=popularity),show.legend = FALSE)+
  coord_flip()+
  theme_classic()+
  labs(x="",y="Popularity",title = "Top 20 Most Popular Anime TV Shows")

tv_anime_airing <- unique_anime%>%filter(type=="TV",airing=="TRUE")
tv_plot_airing <-tv_anime_airing%>%
  mutate(graph_name=paste0(title_english," (",premiered,")"))%>%
  top_n(-20,wt=popularity) %>% 
  ggplot(aes(reorder(graph_name,desc(popularity)),popularity,colour=title_english))+
  geom_point(show.legend = FALSE)+
  geom_segment(aes(x=graph_name,xend=graph_name,y=0,yend=popularity),show.legend = FALSE)+
  coord_flip()+
  theme_classic()+
  labs(x="",y="Popularity",title = "Top 20 Most Popular Anime TV Shows")+
  theme(axis.text.y.left = element_text(size = 12))

tv_anime %>% 
  filter(popularity <= 50) %>%
  mutate(title_english = str_replace(title_english, "Code Geass: Lelouch of the Rebellion", "Code Geass")) %>%
  ggplot(aes(score, scored_by)) + 
  geom_point(shape=21,aes(fill=title_english,size=members)) + 
  geom_text(aes(label = title_english ), check_overlap = T, show.legend = F, size = 3, hjust = 1) + 
  xlim(c(6, 10)) +
  scale_y_log10()+
  labs(title = "Which popular anime also score high?", 
       subtitle = "Top 50 anime shown based on popularity",
       y = "Number of users that scored",
       x = "Score (1-10)") +
  theme_classic()+
  theme(legend.position = 'none',aspect.ratio = 0.5)

tv_anime%>%
  filter(popularity<=100) %>% 
  ggplot(aes(popularity,score,fill=rating))+
  geom_point(shape=21,size=3,alpha=0.8)+
  geom_text(aes(label=title_english),size=3,check_overlap = TRUE)+
  scale_x_log10()+
  theme_classic()+
  scale_color_brewer(palette = 'Set1')+
  theme(legend.position = "bottom")+
  labs(title = "Popularity vs Score",subtitle = "Do all Popular Anime score high?")

most_members <- tv_anime%>%
  top_n(20,wt=members) %>%
  mutate(graph_name=paste0(title_english," (",premiered,")"),graph_name=fct_reorder(graph_name,members))%>%
  ggplot(aes(graph_name,members,fill=graph_name))+
  geom_bar(stat = 'identity',width=0.5,show.legend = FALSE,color='black')+
  coord_flip()+
  theme_classic()+
  scale_y_continuous(limits = c(0,1700000),labels = comma)+
  geom_text(aes(label=comma(members)),size=3)+
  labs(x="",y="Rank",title = "Top 20 Most Members")
most_favorite<-
  tv_anime%>%
  top_n(20,wt=favorites) %>% 
  mutate(title_english=fct_reorder(title_english,favorites))%>%
  ggplot(aes(title_english,favorites,fill=title_english))+
  geom_bar(stat = 'identity',width=0.5,show.legend = FALSE,color='black')+
  coord_flip()+
  theme_classic()+
  scale_y_continuous(limits = c(0,150000),labels = comma)+
  geom_text(aes(label=comma(favorites)),size=3,hjust=1,fontface='bold')+
  labs(x="",y="Favourites",title = "Top 20 Most Favorite")
most_scored<-
  tv_anime%>%
  top_n(20,wt=scored_by) %>% 
  mutate(title_english=fct_reorder(title_english,scored_by))%>%
  ggplot(aes(title_english,scored_by,fill= title_english))+
  geom_bar(stat = 'identity',width=0.5,color='black',show.legend = FALSE)+
  coord_flip()+
  theme_classic()+
  scale_y_continuous(limits = c(0,1500000),labels = comma)+
  geom_text(aes(label=comma(scored_by)),size=3,hjust=1,fontface='bold')+
  labs(x="",y="Favourites",title = "Top 20 Most Scored by")
most_favorite + most_members+ most_scored + plot_layout(widths = 20)

# Calculate correlation coefficients between popularity metrics and user engagement factors
correlation_matrix <- cor(tv_anime[c("popularity", "score", "scored_by", "members", "favorites")])

# Perform linear regression to quantify the impact of independent variables on popularity
model <- lm(popularity ~ score + scored_by + members + favorites, data = tv_anime)
summary(model)

# Summary statistics of the data
summary(tv_anime)
# Mean of popularity column
mean(tv_anime$popularity)
# Median of scored_by column
median(tv_anime$scored_by)
# Standard deviation of members column
sd(tv_anime$members)

install.packages("reshape2")
library(reshape2)
library(ggplot2)

# Convert correlation matrix to data frame
cor_df <- as.data.frame(correlation_matrix)
cor_df$row <- rownames(correlation_matrix)
cor_df <- melt(cor_df, id.vars = "row", variable.name = "column", value.name = "correlation")

# Plot the correlation matrix
correlation_plot <- ggplot(data = cor_df, aes(x = row, y = column, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Matrix")

# Display the correlation plot
correlation_plot


# Create a scatter plot of actual popularity vs. predicted popularity from the linear regression model
prediction_plot <- ggplot(data = tv_anime, aes(x = popularity, y = fitted(model))) +
  geom_point() +
  geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color = "red", linetype = "dashed") +
  labs(x = "Actual Popularity", y = "Predicted Popularity", title = "Linear Regression Model")

# Display the prediction plot
prediction_plot


