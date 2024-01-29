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

p1 <- ggplot(mtcars) +
  geom_point(aes(x = mpg, y = hp), alpha = 4/10, fill = "blue") +
  geom_point(aes(x = mpg, y = wt), alpha = 9/10, fill = "red")

p2 <- ggplot(mtcars) +
  geom_point(aes(x = mpg, y = wt), alpha = 9/10, fill = "red")

p1
p2
