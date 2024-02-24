# Assignment 6 (C. Haut)

library(tidyverse)
library(janitor)
library(gganimate)

dat <- read_csv("../../Data/BioLog_Plate_Data.csv") 

df <- clean_names(dat)

#1. Cleans this data into tidy (long) form
x <- 
  df %>% 
  pivot_longer(starts_with("hr_"),
               names_to = "time",
               values_to = "absorbance") %>% 
  mutate(time = case_when(time == "hr_24"~24,
                          time == "hr_48"~48,
                          time == "hr_144"~144))

# 2. Creates a new column specifying whether a sample is from soil or water

df_tidy <- 
x %>% 
  mutate(sw = x$sample_id) %>% 
  mutate(sw = case_when(sample_id =="Clear_Creek"~"water",
                        sample_id == "Waste_Water"~"water",
                        sample_id == "Soil_1"~"soil",
                        sample_id == "Soil_2"~"soil"))


# 3. Generates a plot that matches this one (note just plotting dilution == 0.1)

df_tidy %>% 
  filter(dilution == 0.1) %>% 
  mutate(sw = as.factor(sw)) %>% 
  ggplot(aes(x = time, y = absorbance, color = sw))+
  geom_smooth(method = "loess", se = FALSE)+
  facet_wrap(~substrate)

# 4. Generates an animated plot that matches this one 
# (absorbance values are mean of all 3 replicates for each group):
# This plot is just showing values for the substrate “Itaconic Acid”

df_tidy %>% 
  filter(dilution == 0.1, substrate == "Itaconic Acid") %>% 
  mutate(sw = as.factor(sw)) %>% 
  ggplot(aes(x = time, y = absorbance, color = sw))+
  geom_smooth(method = "loess", se = FALSE)+
  gganimate()+
  facet_wrap(~substrate)



















