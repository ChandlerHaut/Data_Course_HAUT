##### Modles

library(tidyverse)
library(modelr)
library(easystats)
library(MASS)


names(mpg)

mod1 <- lm(hp~mpg+cly, data = mtcars)
mod2 <- lm(hp~mpg+disp, data = mtcars)

plot(compare_performance(mod1,mod2,rank = TRUE, verbose = FALSE))

# Use mpg data
# build a model that predicts cty as a function of disp

mod1 <- glm(data = mpg,formula = cty~displ)
class(mod1)
mod1$coefficients
mod1$residuals
mod1$formula 

names(mpg)
mpg$cyl
mod1 <- glm(data = mpg,formula = cty~displ)
mod2 <- glm(data = mpg,formula = cty~displ+cyl)
mod3 <- glm(data = mpg,formula = cty~displ*cyl)

#add new column indicating automatice or manual as T/F
mpg <- 
mpg %>% 
  mutate(auto = grepl("auto",trans))

mod4 <- glm(data = mpg,formula = cty~displ*cyl*auto)


compare_models(mod1, mod2,mod3,mod4)
compare_performance(mod1, mod2,mod3) %>%  #AIC smaller is better, R2 closer to 1 is better
  plot()                                  # RMSE (Root Mean Square error) smaller is better
         
summary(mod4)
mpg$pred <- predict(mod1,mpg) #can create the prediction as a new column
mpg$pred2 <- predict(mod2,mpg)
mpg$pred3 <- predict(mod3,mpg)
mpg$pred_best <- predict(mod_best,mpg)

mod5<- glm(data = mpg,formula = cty~manufacturer + model + displ + 
             year + cyl * trans + drv  + fl + class)
formula(mod5)

step <- stepAIC(mod5)
step$formula
?stepAIC
mod_best <- glm(data = mpg, formula = formula(step))

compare_performance(mod1, mod2,mod3,mod4, mod_best) %>%  #AIC smaller is better, R2 closer to 1 is better
  plot() 

check_model(mod_best)



mpg %>% 
  ggplot(aes(x=cty,y=pred))+
  geom_point()

mpg %>% 
  pivot_longer(starts_with("pred")) %>% 
  ggplot(aes(x=displ, y=cty, color = factor(cyl)))+
  geom_point()+
  geom_point(aes(y=value), color = "black")+
  facet_wrap(~name)

summary(mod_best)
predict(mod1, data.frame(displ= 1:100)) %>%  plot

mpg %>% # mod1
  ggplot(aes(x=displ, y=cty))+
  geom_smooth(method = "glm")

mpg %>% # mod3
  ggplot(aes(x=displ, y=cty, color = factor(cyl)))+
  geom_smooth(method = "glm")







