library(tidyverse)
library(latex2exp)
library(ggpubr)
library(icaeDesign)
library(here)

# Wir betrachten die Verletzung aufgrund von Simulatanitaet

# G0 = 60 + 0.5Y
# Y = C + I + G; (I+G)=Z

set.seed(123)
nobs <- 50

CoefMatrix <- matrix(c(-0.5, 1,1,-1), nrow = 2, byrow = 2)

C_0 <- 60 + rnorm(n = nobs, 0, 10)
Z <- runif(n = nobs, min = 30, max = 80) 

exogData <- cbind(C_0, Z) 
endogData <- matrix(data = NA, nrow = nobs, ncol = 2) 

for (i in 1:nobs) {
  endogData[i,] <- solve(CoefMatrix, exogData[i, ]) }

konsumdaten <- tibble(
  Konsum = endogData[, 2],
  Einkommen =  endogData[, 1]
)

konsum_model_lm <- lm(Konsum~Einkommen, data = konsumdaten)

modernes_modell_predictions <- tibble(
  Konsum_predict = predict(
    konsum_model_lm, konsumdaten), 
  Einkommen=konsumdaten$Einkommen) %>%
  dplyr::mutate(
    Konsum_predict_correct = (60 + 0.5*konsumdaten$Einkommen)
  )

cols <- c(
  "Wahr" = "#61a27c",
  "Geschätzt" = "#b8103b"
)
konsum_daten_plot <- ggplot(
  data = konsumdaten, aes(x=Einkommen, y=Konsum)) +
  labs(title = "Konsum und Einkommen") +
  geom_point() +
  theme_icae()

konsum_daten_bias_plot <- konsum_daten_plot +
  labs(title = "Simultanitätsproblematik") +
  geom_line(
    data = modernes_modell_predictions, 
    aes(x=Einkommen, y=Konsum_predict, color = "Geschätzt"),
    key_glyph = draw_key_rect
    
  ) +
  geom_line(
    data = modernes_modell_predictions, 
    aes(x=Einkommen, y=Konsum_predict_correct, color = "Wahr"),
    key_glyph = draw_key_rect
  ) +
  scale_color_manual(values = cols)

ggsave(plot = konsum_daten_plot, 
       filename = here("figures/T10/07_Konsumdaten.pdf"), 
       width = 4, height = 4)

ggsave(plot = konsum_daten_bias_plot, 
       filename = here("figures/T10/08_KonsumBiasReag.pdf"), 
       width = 4, height = 4)
konsum_daten_bias_plot
