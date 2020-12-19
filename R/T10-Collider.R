library(tidyverse)
library(latex2exp)
library(stargazer)
library(ggpubr)
library(here)

# Betrachtung des Collider-Bias

# 1. Einzelfall-Betrachtung----------------------------------------------------

set.seed(123)
nobs <- 100
b0_einkommen <- 500
b1_groesse <- 5
b2_mathe <- 2.5

groesse <- rnorm(n = nobs, mean = 180, sd = 20)
mathe <- runif(n = nobs)
einkommen <- b0_einkommen + b1_groesse*groesse + b2_mathe*mathe + rnorm(n = nobs)

datensatz <- tibble(
  Groesse=groesse,
  MatheSkills=mathe,
  Einkommen=einkommen
)

full_reg <- lm(MatheSkills~Groesse+Einkommen, data = datensatz)
red_reg <- lm(MatheSkills~Groesse, data = datensatz)

summary(full_reg)
summary(red_reg)

stargazer(
  full_reg, red_reg, 
  omit.stat = c("f", "adj.rsq"), 
  float = F, style = "ajps", 
  out=here("figures/T10/collider_bias.tex")
)

# 2.1 Simulations-Betrachtung---------------------------------------------------
set.seed(123)
nobs <- 100
n_simuls <- 5000

b0_einkommen <- 500
b1_groesse <- 5
b2_mathe <- 2.5

estimates_full <- list(
  b0 = rep(NA, n_simuls),
  b1 = rep(NA, n_simuls),
  b2 = rep(NA, n_simuls),
  b0_p = rep(NA, n_simuls),
  b1_p = rep(NA, n_simuls),
  b2_p = rep(NA, n_simuls)
)

estimates_red <- list(
  b0 = rep(NA, n_simuls),
  b1 = rep(NA, n_simuls),
  b0_p = rep(NA, n_simuls),
  b1_p = rep(NA, n_simuls)
)

make_data <- function(){
  groesse <- rnorm(n = nobs, mean = 180, sd = 20)
  mathe <- runif(n = nobs)
  einkommen <- b0_einkommen + b1_groesse*groesse + 
    b2_mathe*mathe + rnorm(n = nobs)
  datensatz <- tibble(
    Groesse=groesse,
    MatheSkills=mathe,
    Einkommen=einkommen
  )
  datensatz
}

for (i in 1:n_simuls){
  datensatz <- make_data()
  full_reg <- summary(lm(MatheSkills~Groesse+Einkommen, data = datensatz))
  red_reg <- summary(lm(MatheSkills~Groesse, data = datensatz))
  estimates_full[["b0"]][i] <- full_reg[["coefficients"]][,1][["(Intercept)"]]
  estimates_full[["b1"]][i] <- full_reg[["coefficients"]][,1][["Groesse"]]
  estimates_full[["b2"]][i] <- full_reg[["coefficients"]][,1][["Einkommen"]]
  estimates_red[["b0"]][i] <- red_reg[["coefficients"]][,1][["(Intercept)"]]
  estimates_red[["b1"]][i] <- red_reg[["coefficients"]][,1][["Groesse"]]
  
  estimates_full[["b0_p"]][i] <- full_reg[["coefficients"]][,4][["(Intercept)"]]
  estimates_full[["b1_p"]][i] <- full_reg[["coefficients"]][,4][["Groesse"]]
  estimates_full[["b2_p"]][i] <- full_reg[["coefficients"]][,4][["Einkommen"]]
  estimates_red[["b0_p"]][i] <- red_reg[["coefficients"]][,4][["(Intercept)"]]
  estimates_red[["b1_p"]][i] <- red_reg[["coefficients"]][,4][["Groesse"]]
 }

# 2.2: Visualisierung----------------------------------------------------------

estimates_full <- as_tibble(estimates_full) %>%
  pivot_longer(cols = everything(), names_to = "estimate", values_to = "value") 

full_estimation <- estimates_full %>%
  filter(estimate=="b1") %>%
  ggplot(data = ., aes(x=value)) +
  geom_histogram(aes(y=..density..), binwidth = 0.025, alpha=0.5) +
  geom_vline(xintercept = 0) +
  labs(
    title = TeX("Schätzungen für $\\beta_1$ (mit Einkommen)"), 
    x="Geschätzter Wert", y="Dichte") +
  scale_y_continuous(expand = expansion()) +
  theme_icae()
full_estimation

estimates_red <- as_tibble(estimates_red) %>%
  pivot_longer(cols = everything(), names_to = "estimate", values_to = "value") 

red_estimation <- estimates_red %>%
  filter(estimate=="b1") %>%
  ggplot(data = ., aes(x=value)) +
  geom_histogram(aes(y=..density..), binwidth = 0.0005, alpha=0.5) +
  geom_vline(xintercept = 0) +
  labs(
    title = TeX("Schätzungen für $\\beta_1$ (ohne Einkommen)"), 
    x="Geschätzter Wert", y="Dichte") +
  scale_y_continuous(expand = expansion()) +
  theme_icae()
red_estimation

collider_bias_full <- ggarrange(red_estimation, full_estimation, ncol = 2)

ggsave(plot = collider_bias_full, 
       filename = here("figures/T10/99_collider.pdf"), 
       width = 8, height = 3)
  