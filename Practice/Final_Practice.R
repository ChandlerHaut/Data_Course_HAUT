library(tidyverse)
bat <- read_csv('./Data/R1_NABat_VettedObservations_NWRS2022.csv')

p1 <- 
bat %>% 
  filter(!is.na(confirmed)) %>% 
  group_by(CommonName, RefugeName) %>% 
  summarise(confirmed = sum(confirmed))

p1 %>% 
  ggplot()+
  geom_point(aes(x= CommonName, y = confirmed))+
  facet_wrap(~RefugeName, scales = 'fixed')+
  theme_classic()+
  theme(axis.text.x  = element_text(angle = 90, size = 5))

bat %>% 
  mutate(ObservationDate = as.Date(ObservationDate, format = "%Y-%m-%d")) %>%
  filter(!is.na(ObservationDate))

p2 <- 
bat %>% 
  filter(!is.na(confirmed)) %>% 
  group_by(CommonName, ObservationDate) %>% 
  summarise(confirmed = sum(confirmed), .groups = 'drop')


p2 %>% 
  ggplot()+
  geom_point(aes(x= ObservationDate, y = confirmed))+
  facet_wrap(~CommonName, scales = 'free')+
  theme_classic()+
  theme(axis.text.x  = element_text(angle = 90, size = 5))



bat %>% 
  ggplot(aes(x= CommonName, y = confirmed))+
  geom_point(fill = "turquoise")+
  facet_wrap(~RefugeName)+
  theme(axis.text.x  = element_text(angle = 90))
                        

