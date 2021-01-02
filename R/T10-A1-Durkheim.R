library(tidyverse)
library(latex2exp)
library(ggpubr)
library(icaeDesign)
library(here)

# Wir nehmen das Beispiel aus T9 zum Zusammenhang zwischen sozialer
# Kohäsion und Selbstmordwahrscheinlichkeit:
set.seed(123)
n_obs <- 1000
soz_koh <- seq(-0.5, 0.5, length.out = n_obs)
p_selbstmord <- soz_koh**2 + rnorm(n_obs, mean = 0, sd = 0.005)

selbstmord_daten <- tibble(
  Zusammenhalt=soz_koh, 
  Selbstmordwahrscheinlichkeit=p_selbstmord
) %>%
  dplyr::mutate(
    Gesellschaft = ifelse(Zusammenhalt<=0, "Modern", "Archaisch")
  )

selbstmord_daten_plot <- ggplot(
  data = selbstmord_daten, 
  mapping = aes(x=Zusammenhalt, y=Selbstmordwahrscheinlichkeit)
) +
  geom_point(alpha=0.25, size=0.2) +
  labs(title = "Selbstmordraten a la Durkheim") +
  theme_icae()

ggsave(plot = selbstmord_daten_plot, 
       filename = here("figures/T10/04_DurkheimDaten.pdf"), 
       width = 6, height = 3)

selbstmord_daten_plot_log <- selbstmord_daten %>%
  mutate(
    Zusammenhalt=log(Zusammenhalt + 0.51),
    Selbstmordwahrscheinlichkeit_log = log(Selbstmordwahrscheinlichkeit+0.05)
    ) %>%
  ggplot(
  data = ., 
  mapping = aes(x=Zusammenhalt, y=Selbstmordwahrscheinlichkeit)
) +
  geom_point(alpha=0.25, size=0.2) +
  labs(title = "Selbstmordraten a la Durkheim (log)", 
       x="Zusammenhalt + 0.51 (log)", 
       y="Selbstmordwahrscheinlichkeit + 0.05 (log)") +
  theme_icae()

ggsave(plot = ggarrange(selbstmord_daten_plot, 
                        selbstmord_daten_plot_log, ncol = 2), 
       filename = here("figures/T10/04_DurkheimDaten_log.pdf"), 
       width = 6, height = 3)

# Schätzung verschiedener Modelle----------------------------------------------

komplettes_modell <- lm(
  Selbstmordwahrscheinlichkeit~Zusammenhalt, data = selbstmord_daten)

komplettes_modell_predictions <- tibble(
  Selbstmord_predict = predict(
    komplettes_modell, selbstmord_daten), 
  Zusammenhalt=selbstmord_daten$Zusammenhalt)

modernes_modell <- lm(
  Selbstmordwahrscheinlichkeit~Zusammenhalt, 
  data = dplyr::filter(selbstmord_daten, Gesellschaft=="Modern")
  )

modernes_modell_predictions <- tibble(
  Selbstmord_predict = predict(
    modernes_modell, selbstmord_daten), 
  Zusammenhalt=selbstmord_daten$Zusammenhalt)

archaisches_modell <- lm(
  Selbstmordwahrscheinlichkeit~Zusammenhalt, 
  data = dplyr::filter(selbstmord_daten, Gesellschaft=="Archaisch")
  )

archaisches_modell_predictions <- tibble(
  Selbstmord_predict = predict(
    archaisches_modell, selbstmord_daten), 
  Zusammenhalt=selbstmord_daten$Zusammenhalt)

quadratisches_modell <- lm(
  Selbstmordwahrscheinlichkeit~poly(Zusammenhalt, 2), 
  data = selbstmord_daten
)

quadratisches_modell_predictions <- tibble(
  Selbstmord_predict = predict(
    quadratisches_modell, selbstmord_daten), 
  Zusammenhalt=selbstmord_daten$Zusammenhalt)

# Grafische Darstellung der kompletten Schätzung-------------------------------
bias_komplettes_modell <- ggplot(
  data = selbstmord_daten, 
  mapping = aes(x=Zusammenhalt, y=Selbstmordwahrscheinlichkeit)
) +
  geom_point(alpha=0.25, size=0.2) +
  geom_line(
    data = komplettes_modell_predictions, 
    aes(x=Zusammenhalt, y=Selbstmord_predict),
    color = "#b8103b"
    ) +
  labs(title = "Lineares Modell für komplette Daten") +
  theme_icae()
bias_komplettes_modell

bias_komplettes_modell_TA <- ggplot(
  data = tibble(
    Fitted = komplettes_modell$fitted.values, 
    Residuen = komplettes_modell$residuals),
  aes(x=Fitted, y=Residuen)
) +
  labs(title = "TA-Plot für komplette Regression") +
  geom_point(alpha=0.5, size=0.2) + 
  theme_icae()
bias_komplettes_modell_TA

plot_komplettes_modell <- ggarrange(
  bias_komplettes_modell, bias_komplettes_modell_TA, ncol = 2
)

ggsave(plot = plot_komplettes_modell, 
       filename = here("figures/T10/04_DurkheimKomplett.pdf"), 
       width = 6, height = 3)

# Grafische Darstellung der getrennten Schätzungen-----------------------------
cols <- c(
  "Archaische G." = "#61a27c",
  "Moderne G." = "#b8103b"
)
bias_getrennes_modell <- ggplot(
  data = selbstmord_daten, 
  mapping = aes(x=Zusammenhalt, y=Selbstmordwahrscheinlichkeit)
) +
  geom_point(alpha=0.25, size=0.2, show.legend = F) +
  geom_line(
    data = modernes_modell_predictions, 
    aes(x=Zusammenhalt, y=Selbstmord_predict, color = "Moderne G."),
    key_glyph = draw_key_rect
    
  ) +
  geom_line(
    data = archaisches_modell_predictions, 
    aes(x=Zusammenhalt, y=Selbstmord_predict, color = "Archaische G."),
    key_glyph = draw_key_rect
  ) +
  scale_y_continuous(
    limits = c(-0.05, 0.3), expand = expansion()
    ) +
  scale_color_manual(values = cols) + 
  labs(title = "Lineare Modelle für getrennte Daten") +
  theme_icae()

bias_getrennt_A_TA <- ggplot(
  data = tibble(
    Fitted = archaisches_modell$fitted.values, 
    Residuen = archaisches_modell$residuals),
  aes(x=Fitted, y=Residuen)
) +
  labs(title = "TA-Plot für archaische Regression") +
  geom_point(alpha=0.5, size=0.2) + 
  theme_icae()

bias_getrennt_M_TA <- ggplot(
  data = tibble(
    Fitted = modernes_modell$fitted.values, 
    Residuen = archaisches_modell$residuals),
  aes(x=Fitted, y=Residuen)
) +
  labs(title = "TA-Plot für moderne Regression") +
  geom_point(alpha=0.5, size=0.2) + 
  theme_icae()

getrennt_full <- ggarrange(
  bias_getrennes_modell, 
  ggarrange(bias_getrennt_A_TA, 
            bias_getrennt_M_TA, nrow = 2), 
  ncol = 2)

ggsave(plot = getrennt_full, 
       filename = here("figures/T10/05_DurkheimGetrennt.pdf"), 
       width = 6, height = 3)

# Grafische Darstellung der quadratischen Schätzung----------------------------

plot_quadratisches_modell <- ggplot(
  data = selbstmord_daten, 
  mapping = aes(x=Zusammenhalt, y=Selbstmordwahrscheinlichkeit)
) +
  geom_point(alpha=0.25, size=0.2) +
  geom_line(
    data = quadratisches_modell_predictions, 
    aes(x=Zusammenhalt, y=Selbstmord_predict),
    color = "#b8103b"
  ) +
  labs(title = "Quadratisches Modell für komplette Daten") +
  theme_icae()
plot_quadratisches_modell

quadratisches_modell_TA <- ggplot(
  data = tibble(
    Fitted = quadratisches_modell$fitted.values, 
    Residuen = quadratisches_modell$residuals),
  aes(x=Fitted, y=Residuen)
) +
  labs(title = "TA-Plot für quadratische Regression") +
  geom_point(alpha=0.5, size=0.2) + 
  theme_icae()
quadratisches_modell_TA

plot_quadratisches_modell_full <- ggarrange(
  plot_quadratisches_modell, quadratisches_modell_TA, ncol = 2
)
ggsave(plot = plot_quadratisches_modell_full, 
       filename = here("figures/T10/06_DurkheimQuadrat.pdf"), 
       width = 6, height = 3)

# Output des quadratischen Modells:

summary(quadratisches_modell)

# Das ist deutlich schwieriger zu interpretieren, da wir es hier mit einem
# nicht-linearen Effekt zu tun haben!
# Mehr Infor: https://www.econometrics-with-r.org/nrf.html

