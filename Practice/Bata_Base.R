library(tidyverse)
library(gapminder)
library(stringr)

df <- read_csv('./Data/R1_NABat_VettedObservations_NWRS2022.csv')

View(df)

names(df)

nb <- #Placed the below code in the origianl bats df to keep my SHIT together
  bats %>% 
  filter(!is.na(CommonName)) %>% 
  mutate(CommonName = str_replace(CommonName, "Townsend\x92s big-eared bat", 
                                  "Townsend big-eared bat"))


bats <- df %>% 
  filter(!is.na(CommonName)) %>% 
  mutate(CommonName = str_replace(CommonName, "Townsend\x92s big-eared bat", 
                                  "Townsend big-eared bat")) %>% 
  select(-"organization_name",-"species_list",-"frame", -"project_name")

bats %>% 
  ggplot(aes(x=CommonName, y = ObservationDate))+
  geom_point(aes(color = admin1))

#figure out what your quesiton is!!!!!!
# We want to see the population size changes over time of the bats, maybe use the animate

df %>% 
  filter(!is.na(ObservationDate)) %>% 
  mutate(ObservationDate = as.factor(ObservationDate)) %>%
  ggplot(aes(x=ObservationDate))+
  geom_bar(aes(color = CommonName))+
  facet_wrap(~admin1, scales = "free")














