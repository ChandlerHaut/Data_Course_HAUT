# Exam 1 (C. Haut)
# I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)
library(tidyverse)
df <- read_csv("./data/cleaned_covid_data.csv")


# II. Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)
A_states <- 
df %>% 
  filter(substr(Province_State, 1, 1) =='A')

# Use the tidyverse suite of packages
# Selecting rows where the state starts with “A” is tricky (you can use the grepl() function or just a vector of those states if you prefer)
# III. Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)
names(A_states)
ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~Province_State, scales = 'free')


# Create a scatterplot
# Add loess curves WITHOUT standard error shading
# Keep scales “free” in each facet



# IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for 
#each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)
state_max_fatality_rate <- 
df %>% 
  filter(!is.na(Case_Fatality_Ratio)) %>% 
  group_by(Province_State) %>% 
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))



# I’m looking for a new data frame with 2 columns:
#   
#   “Province_State”
# “Maximum_Fatality_Ratio”
# Arrange the new data frame in descending order by Maximum_Fatality_Ratio
# This might take a few steps. Be careful about how you deal with missing values!

#  V. Use that new data frame from task IV to create another plot. (20 pts)
#
# X-axis is Province_State
# Y-axis is Maximum_Fatality_Ratio
# bar plot
# x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
# X-axis labels turned to 90 deg to be readable
# Even with this partial data set (not current), you should be able to see that (within these dates), different states had very different fatality ratios.
state_max_fatality_rate$Province_State <- 
factor(state_max_fatality_rate$Province_State, levels = (state_max_fatality_rate$Province_State))

  


ggplot(state_max_fatality_rate) +
  geom_bar(aes(x = Province_State,y = Maximum_Fatality_Ratio), 
           stat = "identity", fill = 'blue') +
  theme(axis.text.x = element_text(angle = 90))
  

  



# VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time
# 
# You’ll need to read ahead a bit and use the dplyr package 
# functions group_by() and summarize() to accomplish this.
Total_Deaths <- 
df %>% 
  group_by(Last_Update) %>% 
  summarise(Cumulative_Deaths = sum(Deaths))

ggplot(Total_Deaths) +
  geom_bar(aes(x=Last_Update, y = Cumulative_Deaths), 
           stat = 'identity', fill = 'red') +
  labs(x = 'Date', y = 'Cumulative Deaths', title = 'Cumulative Deaths in the US')
  


