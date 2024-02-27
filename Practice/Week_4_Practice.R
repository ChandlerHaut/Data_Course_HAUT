data("iris")

iris[seq(2,150,2),]

Sepal.Area = iris$Sepal.Length * iris$Sepal.Width

iris$Sepal.Area = iris$Sepal.Length * iris$Sepal.Width
iris_chr <- iris
for (col in names(iris_chr)) {
  iris_chr[,col ]<- as.character(iris_chr[,col])
}

library(tidyverse)

Big_Sepal <- 
iris %>% 
  filter(Sepal.Area > 20,)

ggplot(iris) +
  geom_bar(aes( x = Sepal.Length, y = Petal.Length))


data("mtcars")

mtcars

ggplot(mtcars) +
  geom_point(aes(x = mpg, y = wt), alpha = 5/10, color = "blue") 

# Convert the folling code expressions into "pipe format"
# to make them more readable:

unique(stringr:: str_to_title(iris$Species))
max(round(iris$Sepal.Length),0)
mean(abs(rnorm(100,0,5)))

iris$Species %>% 
  stringr:: str_to_title() %>% 
  unique()

iris$Sepal.Length %>% 
  round(0) %>% 
  max

rnorm(100,0,5) %>% 
  mean()

library(tidyverse)

?geom_bar()

df <- read_csv('./Data/cleaned_bird_data.csv')
names(df)
ggplot(df, aes(x=Egg_mass, y=tail))+
  geom_point(stat = "identity", color = 'blue') +
  summarise()

install.packages("leaflet")  #used to make interactive maps 
library(tidyverse)
library(palmerpenguins)

#Make an interesting plot of the penguin data
names(x)
ggplot(x, aes(x = species, 
              y = body_mass_g)) +
  geom_boxplot() +
  geom_jitter(height = 0, width = .1, alpha = .2)


penguins %>% 
  ggplot(aes(x=body_mass_g, fill=species))+
  geom_density(alpha =.25)

read_delim("./Data/DatasaurusDozen.tsv")

# group by is before summarise
# facet_wrap makes a different table by choice
# Rule 1: never hide our data
# Rule 2: Tell a story 
# Rule 3: plot before running stats
# Rule 4: Make it easy for people to see
# ggpairs brings your data set forward in mulitple tables in different geoms.
# labs is labels
# strip background is the back lines
library(tidyverse)
install.packages("GGally")
yes
 
penguins %>% 
  ggplot(aes(x = body_mass_g, y = bill_depth_mm))+
  geom_point(aes(color = species))+
  facet_wrap(~island,scales = "free")+
  theme_bw()

library(gapminder)
data("gapminder")

 gapminder %>% 
    filter(lifeExp >75) %>% 
     ggplot(aes(x=year, y = lifeExp))+
     geom_point(aes(color = year)) +
    facet_wrap(~country, scales = 'fixed') +
     theme_bw()
  
             