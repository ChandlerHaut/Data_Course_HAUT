library(tidyverse)


bat <- 
  read_csv('./Data/R1_NABat_VettedObservations_NWRS2022.csv')

bat %>%
  ggplot(aes(x= CommonName, y= RefugeName))+
  geom_point(aes(color = confirmed)) +
  theme(axis.text.x  = element_text(angle = 90))


Abat <- 
bat %>% 
  filter(substr(RefugeName, 1, 1) =='A') 



Abat %>% 
ggplot(aes(x= CommonName))+
  geom_bar() +
  facet_wrap(~ObservationDate)+
  theme(axis.text.x  = element_text(angle = 90))


                          

unique(stringr::str_to_title(iris$Species))
Abat %>% 
  stringr:: str_to_title() %>% 
  unique()

max(round(iris$Sepal.Length),0)

mean(abs(rnorm(100,0,5)))
median(round(seq(1,100,0.01),1))

