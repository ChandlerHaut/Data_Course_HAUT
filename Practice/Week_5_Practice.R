# Week 5

library(tidyverse)
library(ggimage)
library(gganimate)
library(patchwork) # stick plots together

library(gapminder)

# More plotting baby

df <- gapminder

p <- 
df %>% 
  #filter(lifeExp > 77) %>% 
  ggplot(aes(x=year, y = lifeExp))+
  geom_point(aes(size = pop)) +
  #facet_wrap(~continent, scales = 'free') +
  theme(axis.text.x  = element_text(angle = 90, size = 5))


p.dark <- 
p + 
  theme_dark()+
  facet_wrap(~continent)

(p + p.dark) / p.dark + plot_annotation("YeeYee") +
  patchwork::plot_layout(guides = 'collect')
 
p /p.dark + plot_annotation("YeeYee")

df$year %>% range

p3 <- 
  ggplot(df,
         aes(x=gdpPercap, y = lifeExp, color = continent))+
  geom_point(aes(size = pop)) +
  geom_text(aes(label=mycountries))

mycountries <- 
c("Venezuela", "Rwanda", "Nepal", "Iraq", "Afghanistan", "United States")

df <- 
df %>% 
  mutate(mycountries = case_when(country %in% mycountries ~ country)) #This is your friend
  


p3 +
  transition_time(time = year) +
  labs(title = 'Year:{frame_time}') 

anim_save("./Practice/gapminder_animation.gif")
ggsave("./Practice/plot_example.png", plot = p3, width = 6,height = 6, dpi = 200) 
# dpi is resolution, 300 is min for printing










