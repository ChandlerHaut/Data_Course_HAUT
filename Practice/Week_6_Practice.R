bp <- 
  dat %>% 
  select(-starts_with("HR"))
View(bp)

hr <- 
  dat %>% 
  select(-starts_with("BP"))

bp <- 
  bp %>% 
  pivot_longer(starts_with("BP"),
               names_to = "visit",
               values_to = "bp") %>% 
  mutate(visit = case_when(visit == "BP...8"~1,
                           visit == "BP...10"~2,
                           visit == "BP...12"~3)) %>% 
  separate(bp, into = c("systolic","diastolic"))

hr <- hr %>% 
  pivot_longer(starts_with("HR"),
               names_to = "visit",
               values_to = "hr") %>% 
  mutate(visit = case_when(visit =="HR...9"~1,
                           visit =="HR...11"~2,
                           visit =="HR...13"~3))

df <- full_join(hr,bp)

library(janitor)

df <- df %>% clean_names()

df$race %>% unique()

df <- df %>% 
  mutate(race = case_when(race == "Caucasian"|race == "WHITE" ~ "White", TRUE ~ race)) %>% 
  mutate(birthdate = paste(year_birth,month_of_birth,day_birth,sep = "-") %>% 
  as.POSIXct()) %>% 
  mutate(systolic = systolic %>% as.numeric(),
         diastolic = diastolic %>% as.numeric()) %>% 
  select(-pat_id,-month_of_birth,-day_birth,-year_birth)


df <- df %>% 
  mutate(hispanic = case_when(hispanic == "Hispanic" ~TRUE, TRUE ~ FALSE))

df %>% 
  ggplot(aes(x = visit,  color = sex)) +
  geom_path(aes(y = systolic))+
  geom_path(aes(y = diastolic))+
  facet_wrap(~race)

df <- df %>% 
  pivot_longer(cols = c("systolic","diastolic"),
               names_to = "bp_type",
               values_to = "bp")


df %>% 
  ggplot(aes(x = visit,  color = sex)) +
  geom_path(aes(y = hr))+
  facet_wrap(~race)

# DAY 2

library(tidyverse)
library(skimr)
library(janitor)

df <- read_csv("./Data/Bird_Measurements.csv")
skimr::skim(df)


df %>% clean_names()
Male <- 
df %>% 
  select(-starts_with("F"),-starts_with("Unsexed"),-ends_with("N")) %>% 
  mutate(sex = "male")

names(Male) <- names(Male) %>% str_remove("M_")


Female <- 
  df %>% 
  select(-starts_with("M"),-starts_with("Unsexed"), -ends_with("N")) %>% 
  mutate(sex = "female")
names(Female) <- names(Female) %>% str_remove("F_")


Unsexed <- 
  df %>% 
  select(-starts_with("F"),-starts_with("M"), -ends_with("N")) %>% 
  mutate(sex = "NA")
names(Unsexed) <- names(Unsexed) %>% str_remove("Unsexed_")

View(clean)

 clean <- 
   Male %>% 
  full_join(Female) %>% 
  full_join(Unsexed)

 
# Download the Worst Data Sheet off of the class site and play around with it
 
 
 library(readxl)
dat <-  read_xlsx("./Data/messy_bp.xlsx", skip = 3) 

names(bp)

n.vistis <- 
  bp %>% 
  select(starts_with("BP")) %>% 
  length()

which(grepl("^BP", names(bp)))

names(bp)[which(grepl("^BP", names(bp)))] <- paste0("visit",1:n.vistis)



bp <- 
  dat %>% 
  select(-starts_with("HR"))


hr <- 
  dat %>% 
  select(-starts_with("BP"))

bp <- 
  bp %>% 
  pivot_longer(starts_with("visit"),
               names_to = "visit",
               values_to = "bp",
               names_prefix = "visit",
               names_transform = as.numeric) %>% # Turns visit1 = 1 and so on
  separate(bp, into = c("systolic","diastolic"))

hr <- hr %>% 
  pivot_longer(starts_with("visit"),
               names_to = "visit",
               values_to = "hr",
               names_prefix = "visit",
               names_transform = as.numeric)
n.vistis <- 
  hr %>% 
  select(starts_with("HR")) %>% 
  length()

which(grepl("^HR", names(hr)))

names(hr)[which(grepl("^HR", names(hr)))] <- paste0("visit",1:n.vistis)

names(hr)

# Under Data, Datasaurus is a good one to look at as well as Juniper Oils. 
