##### Capstone Mock Results



mock_data <- data.frame(
  Location = rep(c("South Mountain","West Canyon", "Medic Hill", "Cedar Point", "Tickville"), each = 10),  # Trap IDs (repeat 1 to 5 for 10 times each)
  Date = rep(seq(as.Date("2024-01-01"), by = "month", length.out = 5), times = 2),  # Sample dates
  Species = sample(c("Dark Kangaroo Rat", "Deer Mouse","Meadow Voles", "Great Basin Pocket Mouse",
                     "Desert Shrew"), 100, replace = TRUE),  # Sample species
  Weight = round(runif(100, min = 2, max =30), digits = 2),# Sample weights (random uniform distribution)
  Sex = sample(c("Male", "Female"), 50, replace = TRUE), # Sample sex (randomly assigned)
  Tail_Length = round(runif(100, min = 50, max =150), digits = 2)
  )

library(tidyverse)

mock_data %>% 
  ggplot()+
  geom_bar(aes(x = Quantity, color = Species))+
  facet_wrap(~Location)



# Make a data frame for each species and then combine them together. 
# This way we could possibly see a correlation between the species. 

mock_data <- 
  data.frame(Location = rep(c("South Mountain","West Canyon", "Medic Hill", "Cedar Point", "Tickville"), each = 10),
           Species = sample(c("Dark Kangaroo Rat", "Deer Mouse","Meadow Voles", "Great Basin Pocket Mouse",
                              "Desert Shrew"), 100, replace = TRUE),
           Sex = sample(c("Male", "Female"), 50, replace = TRUE),
           Population = round(runif(100, min = 2, max =30), digits = 2))


DM <- 
data.frame(Population = round(runif(100, min = 2, max =30), digits = 2),
           Location = rep(c("South Mountain","West Canyon", "Medic Hill", "Cedar Point", "Tickville"), each = 10),
           Species = "Deer Mouse")

DKR <- 
  data.frame(Population = round(runif(100, min = 2, max =30), digits = 2),
             Location = rep(c("South Mountain","West Canyon", "Medic Hill", "Cedar Point", "Tickville"), each = 10),
             Species = "Dark Kangaroo Rat")
MV <- 
  data.frame(Population = round(runif(100, min = 2, max =30), digits = 2),
             Location = rep(c("South Mountain","West Canyon", "Medic Hill", "Cedar Point", "Tickville"), each = 10),
             Species = "Meadow Voles")
GBPM <- 
  data.frame(Population = round(runif(100, min = 2, max =30), digits = 2),
             Location = rep(c("South Mountain","West Canyon", "Medic Hill", "Cedar Point", "Tickville"), each = 10),
             Species = "Great Basin Pocket Mouse")
DS <- 
  data.frame(Population = round(runif(100, min = 2, max =30), digits = 2),
             Location = rep(c("South Mountain","West Canyon", "Medic Hill", "Cedar Point", "Tickville"), each = 10),
             Species = "Desert Shrew")

master <- combine(DM,DKR,MV, GBPM, DS)

master %>% 
  ggplot(aes(x = Species, y = Population))+
  geom_point()
