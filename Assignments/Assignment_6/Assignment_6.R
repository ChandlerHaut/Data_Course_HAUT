# Assignment 6 (C. Haut)

library(tidyverse)
library(stringr)
library(skimr)
library(janitor)
library(patchwork)
library(gganimate)
library(transformr)


dat <- read_csv("../../Data/BioLog_Plate_Data.csv") 

df <- clean_names(dat)

#1. Cleans this data into tidy (long) form
x <- 
  df %>% 
  pivot_longer(starts_with("hr_"),
               names_to = "time",
               values_to = "absorbance") %>% 
 # mutate(time = case_when(time == "hr_24"~24,
                         # time == "hr_48"~48,
                         # time == "hr_144"~144))
  mutate(time = str_remove(time, "hr_")) %>% 
  mutate(time = as.numeric(time))


# 2. Creates a new column specifying whether a sample is from soil or water

df_tidy <- x %>% 
  mutate(sample_source = case_when(
    sample_id %in% c("Clear_Creek","Waste_Water") ~ "water",
    sample_id %in% c("Soil_1","Soil_2") ~ "soil"))

# 3. Generates a plot that matches this one (note just plotting dilution == 0.1)

df_tidy %>% 
  filter(dilution == 0.1) %>% 
  ggplot(aes(x = time, y = absorbance, color = sample_source))+
  geom_smooth(se = FALSE)+
  facet_wrap(~substrate)

# 4. Generates an animated plot that matches this one 
# (absorbance values are mean of all 3 replicates for each group):
# This plot is just showing values for the substrate “Itaconic Acid”

IA <- 
df_tidy %>% 
  filter(substrate == "Itaconic Acid") %>% 
  group_by(sample_id, dilution, time) %>% 
  summarize(average_absorbance = mean(absorbance))

IA %>% 
  ggplot(aes(x = time, y = average_absorbance, color = sample_id))+
  geom_line()+
  labs(y = "Mean_absorbance")+
  facet_wrap(~dilution)+
  transition_reveal(time)




  



  















