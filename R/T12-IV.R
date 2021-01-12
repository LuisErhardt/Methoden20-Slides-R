# Die IV muss relevant sein: 
# Check ob sie in erster Stufe viel erklärt

library(tidyverse)
library(latex2exp)
library(ggpubr)
library(icaeDesign)
library(AER)
library(here)
library(stargazer)

#' Generierung der Daten fur Keynes-Beispiel
#' 
#' Generiert Daten fur das Keynes Beispiel. Wir gehen von folgender 
#'  Konsumgleichung aus:
#'  C0 = 60 + 0.5Y
#'  Zudem gehen wir von einer geschlossenen VW aus, sodass gilt:
#'  Y = C + I
#'  wobei die Staatsausgaben in I stecken.
#' @param n_obs Groesse der Stichprobe
#' @param true_b0 Wahrer Wert fuer autonomen Konsum
#' @param true_b1 Wahrer Wert fuer Konsumneigung (in `[0,1]`)
#' @return tibble mit `n_obs` Beobachtungen
generate_data <- function(n_obs, true_b0, true_b1){
  
  CoefMatrix <- matrix(c(-true_b1, 1,1,-1), nrow = 2, byrow = 2)
  C_0 <- true_b0 + rnorm(n = n_obs, 0, 10)
  Z <- runif(n = n_obs, min = 30, max = 80) 
  exogData <- cbind(C_0, Z) 
  
  endogData <- t(
    as.matrix(
      apply(exogData, MARGIN = 1, function(x) solve(CoefMatrix, x))
      # Berechnet für jede Zeile in exogData den y-Wert
      )
    )
  
  full_data <- tibble(
    Einkommen = endogData[, 1],
    Konsum = endogData[, 2],
    Investitionen = exogData[, 2] # Das gleiche wie Z
  )
  
  full_data
}

# Einmaliges Beispiel----------------------------------------------------------

set.seed(123)
nobs <- 50
wahres_b0 <- 60
wahres_b1 <- 0.5
konsumdaten <- generate_data(n_obs = nobs, true_b0=wahres_b0, true_b1=wahres_b1)

konsum_model_lm <- lm(Konsum~Einkommen, data = konsumdaten)

modernes_modell_predictions <- tibble(
  Konsum_predict = predict(
    konsum_model_lm, konsumdaten), 
  Einkommen=konsumdaten$Einkommen) %>%
  dplyr::mutate(
    Konsum_predict_correct = (wahres_b0 + wahres_b1*konsumdaten$Einkommen)
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

ggsave(plot = konsum_daten_bias_plot, 
       filename = here("figures/T12/Keynes_Bias.pdf"), 
       width = 4, height = 4)


# MCS und Vergleich mit IV-----------------------------------------------------

set.seed(123)
mcs_runs <- 5000
nb_obs <- 50
wahres_b0 <- 60
wahres_b1 <- 0.5

einkommen_ols <- rep(NA, mcs_runs)
einkommen_iv <- rep(NA, mcs_runs)

for (i in 1:mcs_runs){
  datensatz <- generate_data(
    n_obs = nb_obs, true_b0 = wahres_b0, true_b1 = wahres_b1)
  ols_schaetzung <- lm(Konsum~Einkommen, data = datensatz)
  iv_schaetzung <- ivreg(
    Konsum~Einkommen | Investitionen, 
    data = datensatz)
  einkommen_ols[i] <- ols_schaetzung[["coefficients"]][2]
  einkommen_iv[i] <- iv_schaetzung[["coefficients"]][2]
}

ols_plot <- ggplot(
  data = tibble(einkommen_ols), mapping = aes(x=einkommen_ols)
  )  +
  geom_histogram(
    aes(y=..density..),
    bins = 50, alpha=0.5, color=NA, fill="#0059b3") +
  geom_vline(xintercept = wahres_b1) +
  labs(title = "Schätzung mit OLS",
       y = "Dichte", 
       x = TeX("$\\hat{\\beta}_1$")) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05))) +
  theme_bw() + theme(
    panel.border = element_blank(), axis.line = element_line()
  )
ols_plot

iv_plot <- ggplot(
  data = tibble(einkommen_iv), mapping = aes(x=einkommen_iv)
)  +
  geom_histogram(
    aes(y=..density..),
    bins = 50, alpha=0.5, color=NA, fill="#0059b3") +
  geom_vline(xintercept = wahres_b1) +
  labs(
    title = "Schätzung mit IV",
    y = "Dichte", 
    x = TeX("$\\hat{\\beta}_1$")) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05))) +
  theme_bw() + theme(
    panel.border = element_blank(), axis.line = element_line()
  )
iv_plot

ggsave(plot = ols_plot, 
       filename = here("figures/T12/Keynes_OLS.pdf"), 
       width = 5, height = 4)

ggsave(plot = iv_plot, 
       filename = here("figures/T12/Keynes_IV.pdf"), 
       width = 5, height = 4)

# Schätzergebnis für die Slides:

stargazer(
  lm(Einkommen~Investitionen, data=datensatz),
  omit.stat = c("f", "adj.rsq"), 
  float = F, style = "ajps", 
  out=here("figures/T12/KeynesIV.tex")
)
