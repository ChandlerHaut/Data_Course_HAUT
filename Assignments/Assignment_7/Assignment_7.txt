#### Assignment 7 C. Haut 
library(tidyverse)
library(janitor)
library(easystats)
library(MASS)



#Import the Assignment_7/Utah_Religions_by_County.csv data set
df <- read_csv("./Utah_Religions_by_County.csv")
# It seems that we have a data set by county with the total population in the year 2010.
# The religious and non-religious columns add up to 100%, and then the denominations are 
# percentages of the religious population. 

#Clean it up into “tidy” shape
p1 <- 
  df %>% 
  pivot_longer(matches("Religious","Non-Religious"),
               names_to = "belief",
               values_to = "percentage")

names(df)

Faith <- c("Assemblies of God","Episcopal Church","Pentecostal Church of God","Greek Orthodox",             
           "LDS","Southern Baptist Convention","United Methodist Church","Buddhism-Mahayana",          
           "Catholic","Evangelical","Muslim","Non Denominational","Orthodox" )  

p2 <- 
  p1 %>% 
  pivot_longer(matches(Faith),
               values_to = "faith_per",
               names_to = "denomination")

#I decided that I didn't need to mess with the religious and non-religous columns
p3 <- 
  df %>% 
  pivot_longer(matches(Faith),
               values_to = "faith_per",
               names_to = "denomination") %>% 
  mutate(non_religious = `Non-Religious`)



#Explore the cleaned data set with a series of figures (I want to see you exploring the data set)

#“Does population of a county correlate with the 
#proportion of any specific religious group in that county?”

mod1 <- glm(data= p3, formula = faith_per ~ Pop_2010)
mod2 <- glm(data= p3, formula = faith_per ~ Pop_2010 + non_religious)
mod3 <- glm(data= p3, formula = faith_per ~ Pop_2010 * non_religious)
mod4 <- glm(data= p3, formula = faith_per ~ Pop_2010 * County + non_religious)
compare_performance(mod1,mod2,mod3,mod4) %>% plot

# through this mod1 looks like the best one overall

mod5 <- glm(data= p3, formula = faith_per ~ Pop_2010 + non_religious + denomination + County + Religious)
stepAIC(mod5)
mod6 <- glm(data= p3, formula = faith_per ~ denomination)

plot1 <- 
p3 %>% 
  ggplot(aes(x = denomination, y = faith_per))+
  geom_point(aes(color = denomination))+
  theme_dark()
#This plot shows that if a county has an LDS population present then it is above 25%.


p4 <- 
  p3 %>% 
  filter(faith_per >= .1) 

plot2 <- 
p4%>% 
  ggplot(aes(x = Pop_2010, y = faith_per))+
  geom_point(aes(color = denomination))+
  theme_dark()

# I set up the this plot to look at the percentage of the population that are religious by 
# the denomination they reside in (faith_per). I filtered to see any denomination above 10% of the 
# religious population. We have three points to Catholics and the majority to LDS 


#“Does proportion of any specific religion in a given county 
# correlate with the proportion of non-religious people?”
mod7 <- glm(data= p3, formula = non_religious~faith_per + denomination)
mod8 <- glm(data = p3,formula = Pop_2010 ~ non_religious * faith_per)
compare_performance(mod7,mod8) %>% plot

plot3 <- 
p3 %>% 
  ggplot(aes(x = non_religious, y = faith_per))+
  geom_point(aes(color = denomination))+
  theme_dark()

# Yes, there is a correlation between non_religious and the LDS.   
