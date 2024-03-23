library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)
library(patchwork)

#Write a script that:
#1.loads the “/Data/mushroom_growth.csv” data set
df <- read_csv("./mushroom_growth.csv")

#2.creates several plots exploring relationships between the response and predictors
df %>% 
  ggplot(aes(x= Light, y = GrowthRate, color = Species))+
  geom_point()+
  geom_smooth(method = "lm")

df %>% 
  ggplot(aes(x= Temperature , y = GrowthRate, color = Species))+
  geom_jitter()+
  geom_smooth(method = "lm")

df %>% 
  ggplot(aes(x = Nitrogen, y = GrowthRate, color = Species))+
  geom_point()+
  geom_smooth(method = "lm")

#3.defines at least 4 models that explain the dependent variable “GrowthRate”
mod1 <- glm(data = df, formula = GrowthRate ~ Light)
mod2 <- glm(data = df, formula = GrowthRate ~ Light + Humidity)
mod3 <- glm(data = df, formula = GrowthRate ~ Light + Humidity + Nitrogen)

mod_best <- stepAIC(mod3)
formula(mod_best)
#4.calculates the mean sq. error of each model
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)

mean(mod_best$residuals^2)

#5.selects the best model you tried
compare_performance(mod2, mod3,mod_best)

#6.adds predictions based on new hypothetical values for the independent variables used in your model
pred_df <- 
  df %>% 
  add_predictions(mod3)


newdf <- data.frame(Light = rep(c(0,10,20), each = 3), Humidity = rep(c("Low","High"), each = 9), 
                    Nitrogen = rep(c(0,5,10,20,25,30,35,40,45)))


pred <- predict(mod3,newdata = newdf)

hyp_pred <- data.frame(Light = newdf$Light,
                       Humidity = newdf$Humidity,
                       Nitrogen = newdf$Nitrogen,
                       GrowthRate = pred)

df$PredictionType <- "Real"
hyp_pred$PredictionType <- "Hypothetical"

fullpreds <- full_join(df,hyp_pred)


#7.plots these predictions alongside the real data
ggplot()+
  geom_point(data = df, aes(x = Nitrogen, y = GrowthRate, color = Species)) +
  geom_point(data = hyp_pred, aes(x = Nitrogen, y = GrowthRate), color = "black")






