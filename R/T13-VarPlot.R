library(tidyverse)
library(here)

# Werte inspiriert von:
# https://arxiv.org/pdf/1910.05219.pdf

Stichprobengroesse <- seq(1, 6, length.out = 12)
Standardabweichung <- Stichprobengroesse*10 + rnorm(12, sd = 2)

var_plot <- ggplot(
  data = tibble(
    Stichprobengröße=Stichprobengroesse,
    Standardabweichung=Standardabweichung
  ), 
  mapping = aes(x=Stichprobengröße, y=Standardabweichung )
  ) +
  labs(title = "Varianz bei endlastig verteilten Größen") +
  geom_point() + scale_x_continuous(
    labels = scales::number_format(scale = 1000)) +
  theme_bw() 

ggsave(plot = var_plot, filename = here("figures/T13/T13-Varplot.pdf"), 
       height = 3, width = 4)
