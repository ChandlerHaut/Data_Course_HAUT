library(tidyverse)
library(janitor)
library(stringr)
library(easystats)
library(MASS)
library(modelr)



#1. Read in the unicef data (10 pts) 
df <- read_csv("./unicef-u5mr.csv")

# 2. Get it into tidy format (10 pts) 
df_tidy <- 
df %>% 
  pivot_longer(starts_with("U5MR"),
               values_to = "U5MR",
               names_to = "year") %>% 
  mutate(year = str_remove(year, "U5MR.")) %>% 
  mutate(year = as.numeric(year)) 


# 3. Plot each country’s U5MR over time (20 points)
Plot_1 <- 
df_tidy %>% 
  ggplot(aes(x= year, y = U5MR))+
  geom_path()+
  facet_wrap(~Continent) +
  theme_bw()

# 4. Save this plot as LASTNAME_Plot_1.png 

ggsave("Haut_Plot_1.png")


# 5. Create another plot that shows the mean U5MR for all the 
# countries within a given continent at each year (20 pts)
Plot_2 <- 
df_tidy %>% 
  filter(!is.na(U5MR)) %>% 
  group_by(Continent, year) %>% 
  summarise(Mean_U5MR = mean(U5MR)) %>% 
  ggplot(aes(x=year, y = Mean_U5MR, color = Continent))+
  geom_path()+
  theme_bw()

# 6. Save that plot as LASTNAME_Plot_2.png (5 pts)
ggsave("Haut_Plot_2.png")

# 7. Create three models of U5MR (20 pts)

mod1 <- glm(data = df_tidy, formula = U5MR~year) 
mod2 <- glm(data = df_tidy, formula = U5MR~year+Continent)
mod3 <- glm(data = df_tidy, formula = U5MR~year*Continent)

#8. Compare the three models with respect to their performance
compare_performance(mod1, mod2,mod3) %>% plot

# Based off the compare_performance, mod3 is the best model of these three.  

# 9. Plot the 3 models’ predictions like so: (10 pts)

df_tidy$mod1 <- predict(mod1, df_tidy)
df_tidy$mod2 <- predict(mod2, df_tidy)
df_tidy$mod3 <- predict(mod3, df_tidy)


Predicted <- 
df_tidy %>% 
  pivot_longer(starts_with("mod"),
               values_to = "Predicted_U5MR") %>% 
  ggplot(aes(x=year, y = Predicted_U5MR, color = Continent))+
  geom_smooth(method = "glm")+
  facet_wrap(~name)+
  labs(title = "Model Predictions")



#10. BONUS - Using your preferred model, predict what the U5MR would be for 
# Ecuador in the year 2020. The real value for Ecuador for 2020 was 
# 13 under-5 deaths per 1000 live births. How far off was your model prediction???

mod4 <- glm(U5MR ~ year, data = df_tidy)

Ecuador_2020 <- data.frame(year = 2020, CountryName = "Ecuador")

Pred_Ecuador <- predict(mod4, newdata = Ecuador_2020)

# Model Prediction Reality
# mod4  11.44224    13     Not to far off but below the reality. 



