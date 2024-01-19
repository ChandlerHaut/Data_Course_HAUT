x <- c(1,2,3,4,5)

for (i in x) {
  print(i+1)
}


a <- 1:10
b <-  2:11
c <-  letters[1:10]
head(letters,n=10)

a
b
c
d <- rep(TRUE,10)
z <- data.frame(a,b,c,d)
class(z)
length(z)
dim(z)


z[5>a,]

z[c=="b",]

iris

big_iris <- iris[iris$Sepal.Length >5,]

big_iris$Sepal.Area <- big_iris$Sepal.Length * big_iris$Sepal.Width

big_setosa <- big_iris[big_iris$Species == "setosa",]
mean(big_setosa$Sepal.Area)
plot(big_setosa$Sepal.Length,big_setosa$Sepal.Width)
sd()
install.packages("qrcode"
  
)
library(qrcode)

big_iris$Petal.Area <- big_iris$Petal.Length * big_iris$Petal.Width

big_iris(Petal.Area > 5)

class(big_iris)
big_iris[5>Petal.Area,]


big_iris[big_iris$Sepal.Area>20,big_iris$Species == setosa]

Sepal20<- big_iris[big_iris$Sepal.Area>20,]

Sepal20[Sepal20$Species=="virginica",]




