mtcars <- mtcars


cly4 <- mtcars[mtcars$cyl > 4,]

mean(cly4$mpg)
min(cly4$mpg)
max(cly4$mpg)

# object types ####
## logical ####
c(TRUE, TRUE, FALSE)
## numeric ####
1:10
## integer ####
c(1L,2L,3L)
## character ####
letters[3]
## data.frame ####
mtcars[rows, columns]
## factor ####
as.factor(letters)
# annoying but useful
haircolors <- c("brown", "blonde", "black", "red", "red", "black")
as.character(as.factor(haircolors), "purple")



# type conversions ####
1:5 # numeric
as.character(1:5) # convert to character
as.numeric(letters) # you get NA for all 
as.numeric(c("1",'b','35'))
x <- as.logical(c("true","t","F","False","T"))
sum(x,na.rm = TRUE) # HOW TO REMOVE THE NA'S


# data frames ####
str(mtcars) #structure
names(mtcars) #pulls up column names
as.character(mtcars$mpg)
# for-loop assigns character version of every column over itself
for (col in names(mtcars)) {
  mtcars[,col] <- as.character(mtcars[,col])
}
str(mtcars)
data("mtcars") # resets the built-in data set back to normal

path <- "./Data/cleaned_bird_data.csv"
df <- read.csv(path)
str(df)
#convert all columns to characters
for (col in names(df)) {
  df[,col] <- as.character(df[,col])
}
str(df)
View(df)
# write the new file to your computer
write.csv(df,file = "./Data/clearned_bird_data_chr.csv")

# 'apply' family functions ####
apply(mtcars,2,as.factor) # the number is 1 or 2 and refers to rows and columns

lapply(list, function)
sapply(list, function)
vapply(list, function, FUN.VALUE = type, ...)

# Packages ####

## tidyverse ####
library(tidyverse)
#filter helps us subset data freames by rows
mtcars %>% 
  filter(mpg > 19)

# pipe ####
## %>% %>% %>% %>%  
# thing on the left becomes first argument to thing on right 
mtcars$mpg %>% mean()

abs(mean(mtcars$mpg))


library(tidyverse)
library(palmerpenguins) 

x <- penguins %>% 
  filter(bill_length_mm > 40 & sex == "female")
x$body_mass_g %>% mean


# find mean body mass of female long-beaked penguins
penguins %>% 
  filter(bill_length_mm > 40 & sex == "female") %>% 
  pluck("body_mass_g") %>% 
  mean


# Do the same but for each species 
penguins %>% 
  filter(bill_length_mm > 40 & sex == "female") %>% 
  group_by(species,island) %>% # commas to separate all grouping columns
  summarize(mean_body_mass = mean(body_mass_g),
            min_body_mass = min(body_mass_g),
            max_body_mass = max(body_mass_g),
            sd_body_mass = sd(body_mass_g),
            N = n()) %>%  # N = n() is a special function inside summarize
  arrange(desc(mean_body_mass)) %>% 
  write_csv("./Data/penguin_summary.csv")


# find the fattie penguins (body mass > 5000)
# count how many are male and how many are female
# return the max body mas for males and females
# bonus: add a new column to penguins that says whether they're a fattie
penguins %>% 
  filter(body_mass_g > 5000) %>% 
  group_by(sex) %>% 
  summarize(max_fattie = max(body_mass_g), N = n())

penguins %>% #mutate is for making new columns
  mutate(fattie = body_mass_g > 5000)

x <- 
penguins %>% # case_when only works in mutate
  mutate(fatstat = case_when(body_mass_g > 5000 ~ "fattie", 
                             body_mass_g <= 5000 ~ "skinny"))

x %>% 
  filter(!is.na(sex)) %>% 
  ggplot(mapping = aes(x=body_mass_g,
                       y=bill_length_mm, 
                       color = fatstat,
                       shape = fatstat)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = c("turquoise","salmon")) +
  # scale_color_viridis_d(option = "plasma", end = .8) +
    theme_dark()+
  theme(axis.text = element_text(angle = 180, face = 'italic'))

  























