library(tidyverse)
library(easystats)
library(palmerpenguins)


# does body_mass_g vary significantly between penguin species?


mod <- glm(data = penguins, formula = body_mass_g~species) #we have used a numeric continous variable (Body_mass_g)
summary(mod)

#lets predict whether a specis is "Gentoo" based off measurements
names(penguins)

mod2 <- 
penguins %>% 
  mutate(gentoo = case_when(species == "Gentoo"~TRUE,
                            TRUE~FALSE)) %>% 
  glm(data = .,     #dot is the thing you pipe
      formula = gentoo ~ bill_length_mm + body_mass_g +flipper_length_mm + bill_depth_mm,
      family = "binomial") #logistic regression: outcome needs to be T/F

summary(mod2)

penguins$pred <- predict(mod2,penguins, type ="response") #put predictions on scale of response variable

penguins %>% 
  ggplot(aes(x= body_mass_g, y=pred, color = species))+
  geom_point()

preds <- 
penguins %>% 
  mutate(outcome = case_when(pred<0.01 ~ "Not gentoo",
                             pred >0.75 ~ "Gentoo")) %>% 
  select(species, outcome) %>%
  mutate(correct = case_when(species == "Gentoo" & outcome =="Gentoo"~TRUE, 
                             species!= "Gentoo" & outcome == "Not gentoo"~TRUE,
                             TRUE ~ FALSE))

preds %>% 
  pluck("correct") %>% #This is how you check the accuracy of the above code.
  sum() / nrow(preds)

"bill_length_mm"    "bill_depth_mm"    
"flipper_length_mm" "body_mass_g"


dat <- read_csv("./Data/GradSchool_Admissions.csv")
str(dat)

mod3 <- 
  glm(data = dat, 
      formula = as.logical(admit) ~ (gre + gpa) * rank,
      family =  "binomial")
dat$pred <- predict(mod3, dat, type = "response")

dat %>% 
  ggplot(aes(x=gre, y = pred, color = factor(rank)))+
  geom_point(alpha=.25)+
  geom_smooth()

dat %>% 
  ggplot(aes(x=factor(rank), y = pred, color = factor(rank)))+
  geom_jitter(alpha=.25)+
  geom_boxplot()

library(neuralnet) #train to look at pictures
library(keras) #This is a can of worms














