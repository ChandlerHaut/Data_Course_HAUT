library(tidyverse)
library(gapminder)
library(stringr)
library(leaflet)
library(sf)
library(ggspatial)
library(ggmap)


random_sample <- sample(0:5, size = 100, replace = TRUE)
species <- sample(c("dark kangaroo mouse","deer mouse","vole","house mouse","chiseled tooth"), size = 100, replace = TRUE)
location <- rep(c("South Mountain","West Canyon", "Medic Hill", "Cedar Point", "Tickville"), each = 20)

mock_data <- data.frame(location = location, species = species, count = random_sample )
set.seed(7)

nmd <- 
mock_data %>% 
  mutate(percent = count / sum(count)) 

nmd%>% 
  ggplot(aes(x = species, y = percent, fill = species)) +
  geom_bar(stat = "identity")+
  stat_summary(
    aes(label = paste0(sprintf("%.2f", ..y..), "%")),
    fun.y = "sum",
    geom = "text",
    vjust = 0,
    color = "black"
  )  +
  scale_fill_viridis_d()+
  labs(title = "Relative Abundance of Species",
       x = "Species",
       y = "Relative Abundance")



totals <- 
nmd %>% 
  group_by(species) %>% 
  summarise(sum = sum(count))

South_Mountian <- 
nmd%>% 
  filter(location == "South Mountain") %>% 
  ggplot(aes(x = species, y = percent, fill = species)) +
  geom_bar(stat = "identity")+
  stat_summary(
    aes(label = paste0(sprintf("%.2f", ..y..), "%")),
    fun.y = "sum",
    geom = "text",
    vjust = 0,
    color = "black"
  )  +
  scale_fill_viridis_d()+
  labs(title = "Relative Abundance of Species - South Mountain",
       x = "Species",
       y = "Relative Abundance")


West_Canyon <- 
nmd%>% 
  filter(location == "West Canyon") %>% 
  ggplot(aes(x = species, y = percent, fill = species)) +
  geom_bar(stat = "identity")+
  stat_summary(
    aes(label = paste0(sprintf("%.2f", ..y..), "%")),
    fun.y = "sum",
    geom = "text",
    vjust = 0,
    color = "black"
  )  +
  scale_fill_viridis_d()+
  labs(title = "Relative Abundance of Species - West Canyon",
       x = "Species",
       y = "Relative Abundance")

unique(nmd$location)

Medic_Hill <- 
  nmd%>% 
  filter(location == "Medic Hill") %>% 
  ggplot(aes(x = species, y = percent, fill = species)) +
  geom_bar(stat = "identity")+
  stat_summary(
    aes(label = paste0(sprintf("%.2f", ..y..), "%")),
    fun.y = "sum",
    geom = "text",
    vjust = 0,
    color = "black"
  )  +
  scale_fill_viridis_d()+
  labs(title = "Relative Abundance of Species - Medic Hill",
       x = "Species",
       y = "Relative Abundance")

Cedar_Point <- 
  nmd%>% 
  filter(location == "Cedar Point") %>% 
  ggplot(aes(x = species, y = percent, fill = species)) +
  geom_bar(stat = "identity")+
  stat_summary(
    aes(label = paste0(sprintf("%.2f", ..y..), "%")),
    fun.y = "sum",
    geom = "text",
    vjust = 0,
    color = "black"
  )  +
  scale_fill_viridis_d()+
  labs(title = "Relative Abundance of Species - Cedar Point",
       x = "Species",
       y = "Relative Abundance")

Tickville <- 
  nmd%>% 
  filter(location == "Tickville") %>% 
  ggplot(aes(x = species, y = percent, fill = species)) +
  geom_bar(stat = "identity")+
  stat_summary(
    aes(label = paste0(sprintf("%.2f", ..y..), "%")),
    fun.y = "sum",
    geom = "text",
    vjust = 0,
    color = "black"
  )  +
  scale_fill_viridis_d()+
  labs(title = "Relative Abundance of Species - Tickville",
       x = "Species",
       y = "Relative Abundance")

