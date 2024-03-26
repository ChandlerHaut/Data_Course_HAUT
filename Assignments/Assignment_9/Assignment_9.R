library(tidyverse)
library(modelr)
library(easystats)
library(GGally)


df <- read_csv("./GradSchool_Admissions.csv")
view(df)

df %>% 
  ggpairs()

mod1 <- glm(data = df, formula = as.logical(admit) ~ gre + rank)
mod2 <- glm(data = df, formula = as.logical(admit) ~ gre * gpa)
full_mod <- glm(data = df, formula = as.logical(admit) ~ gre * gpa * rank)

compare_performance(mod1,mod2,full_mod)

step <- MASS::stepAIC(full_mod)
mod3 <- glm(data = df, formula = step$formula)

final_comp <- compare_performance(mod1,mod2,full_mod, mod3, rank = TRUE)
mod3$formula

df %>% 
  gather_predictions(mod3) %>% 
  ggplot(aes(x=gpa, y = pred, color = factor(rank)))+
  geom_point()+
  geom_smooth(method = "glm")

df %>% 
  gather_predictions(mod3) %>% 
  ggplot(aes(x=gre, y = pred, color = factor(rank)))+
  geom_point()+
  geom_smooth(method = "glm")
  






