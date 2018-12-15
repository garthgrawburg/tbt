library(tidyverse)
library(gifski)
library(gganimate)
library(ggrepel)
library(here)

imp <- read_csv("Week 41 - Beer Production - 1950 to 2017.csv", 
                skip = 1,
                col_names = c("year", "rank", "brewer", "brewer_barrels", "total_barrels"),
                col_types = cols(
                    year = col_integer(),
                    rank = col_factor(),
                    brewer = col_factor(),
                    brewer_barrels = col_integer(),
                    total_barrels = col_integer())) %>% 
  data.frame() %>% 
  mutate(brewer = str_wrap(brewer, width = 10), 
         pct = brewer_barrels / total_barrels)

gf <- imp %>% ggplot(aes(x = rank, y = pct, size = brewer_barrels, colour = brewer)) +
  theme(panel.grid = element_blank()) +
  geom_text_repel(aes(label = brewer), show.legend = FALSE, max.iter = 5) +
  ylab("Percent of Total Barrels") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.6)) +
  labs(title = 'The Rise of AB: Brewery Concentration: 1950 - 2017

                Year: {closest_state}',
       x = 'Rank', 
       y = 'Percent of Total Barrels',
       caption = 'Size Represents Brewer Barrels
                  Source:  http://www.beerhistory.com/library/holdings/shakeout.shtml') +
  transition_states(year, transition_length = 2, state_length = 7)

animate(gf)
  
anim_save("beer_animation.gif", gf, width = 800, height = 600, units = "px")