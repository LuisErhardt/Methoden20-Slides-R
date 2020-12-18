library(tidyverse)
library(latex2exp)
library(ggpubr)

# 1. Die einfachstmögliche MCS-------------------------------------------------

# Der Zufallsprozess: Zug einer einzenlen normalverteilten ZV:

set.seed(123) # Erlaubt Replikation der Zufallsexperimente
rnorm(n = 1, mean = 0, sd = 1)
set.seed(123)
rnorm(n = 1, mean = 0, sd = 1)

# Dieses 'ZE' wollen wir nun mehrmals durchführen:

n_trials <- 10
results <- rep(NA, n_trials) # Einen leeren Vektor erstellen
for (i in 1:length(results)) {
  print(paste("Aktueller Wert für i:", i))
  einzelne_realisation <- rnorm(n = 1, mean = 0, sd = 1)
  results[i] <- einzelne_realisation
}

# Darstellung der Ergebnisse und Vergleich mit der analytischen Lösung:

ggplot(data = tibble(results), mapping = aes(x=results)) +
  geom_histogram(
    aes(y=..density..),
    bins = 10, 
    alpha=0.5) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05))) +
  theme_bw() + theme(
    panel.border = element_blank(), axis.line = element_line()
  )


# Ein richtige MCS sollte aber schon häufiger durchgeführt werden:
set.seed(123)
n_trials <- 5000
results <- rep(NA, n_trials) # Einen leeren Vektor erstellen
for (i in 1:length(results)) {
  einzelne_realisation <- rnorm(n = 1, mean = 0, sd = 1)
  results[i] <- einzelne_realisation
}

# Darstellung der Ergebnisse und Vergleich mit der analytischen Lösung:

ggplot(data = tibble(results), mapping = aes(x=results)) +
  geom_histogram(
    aes(y=..density..),
    binwidth = 0.2, alpha=0.5) +
  stat_function(
    fun = dnorm, 
    args = list(mean = 0, sd = 1), 
    color="blue") +
  scale_y_continuous(expand = expansion(add = c(0, 0.05))) +
  theme_bw() + theme(
    panel.border = element_blank(), axis.line = element_line()
  )

# Natürlich könnte man diese MCS auch einfach haben:
set.seed(123)
resultat_einfach <- rnorm(n = n_trials, mean = 0, sd = 1)
all.equal(results, resultat_einfach)

# Zudem kann man die - intuitiveren - Schleifen auch effizienter haben:



# 2. Eine sinnvollere MCS: der zentrale Grenzwertsatz--------------------------

n_trials <- 5000
resultat <- rep(NA, n_trials)
for (i in 1:length(resultat)){
  zv_zug <- runif(n = 100)
  zv_mittelwert <- mean(zv_zug)
  resultat[i] <- zv_mittelwert
}

# Darstellung der Ergebnisse:

ggplot(data = tibble(resultat), mapping = aes(x=resultat)) +
  geom_histogram(
    aes(y=..density..),
    binwidth = 0.01, 
    alpha=0.5) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05))) +
  theme_bw() + theme(
    panel.border = element_blank(), axis.line = element_line()
  )

# 3. Analyse des OLS-Schätzers--------------------------
set.seed(1)
simulationen <- 5000
sample_size <- 500
b0 <- 1
b1 <- 2
b2 <- 4

b0_schaetzungen <- rep(NA, simulationen)
b1_schaetzungen <- rep(NA, simulationen)
b2_schaetzungen <- rep(NA, simulationen)

for (i in 1:simulationen){
  # Erstelle Daten:
  x1_var <- runif(n = sample_size, min = 2, max = 5)
  x2_var <- runif(n = sample_size, min = 5, max = 10)
  errors <- rnorm(n = sample_size, mean = 0, sd = 1)
  y_var <- b0 + b1*x1_var + b2*x2_var + errors
  
  datensatz <- tibble(
    x1=x1_var, x2=x2_var, yvar=y_var
  )
  
  # Schaetze Modell:
  model_est <- lm(yvar ~ x1 + x2, data = datensatz)
  
  # Speichere Ergebnisse:
  b0_schaetzungen[i] <- unname(model_est[["coefficients"]]["(Intercept)"])
  b1_schaetzungen[i] <- unname(model_est[["coefficients"]]["x1"])
  b2_schaetzungen[i] <- unname(model_est[["coefficients"]]["x2"])
}

# Darstellung der Ergebnisse:

b0plot <- ggplot(
  data = tibble(schaetzung = b0_schaetzungen), 
  mapping = aes(x=schaetzung)) +
  geom_histogram(
    aes(y=..density..),
    binwidth = 0.1, 
    alpha=0.5) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05))) +
  geom_vline(xintercept = b0) +
  labs(title = TeX("Schätzer für $\\beta_0$")) +
  theme_bw() + theme(
    panel.border = element_blank(), axis.line = element_line()
  )

b1plot <- ggplot(
  data = tibble(schaetzung = b1_schaetzungen), 
  mapping = aes(x=schaetzung)) +
  geom_histogram(
    aes(y=..density..),
    binwidth = 0.005, 
    alpha=0.5) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05))) +
  geom_vline(xintercept = b1) +
  labs(title = TeX("Schätzer für $\\beta_1$")) +
  theme_bw() + theme(
    panel.border = element_blank(), axis.line = element_line()
  )

b2plot <- ggplot(
  data = tibble(schaetzung = b2_schaetzungen), 
  mapping = aes(x=schaetzung)) +
  geom_histogram(
    aes(y=..density..),
    binwidth = 0.005, 
    alpha=0.5) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05))) +
  geom_vline(xintercept = b2) +
  labs(title = TeX("Schätzer für $\\beta_2$")) +
  theme_bw() + theme(
    panel.border = element_blank(), axis.line = element_line()
  )

full_plot <- ggarrange(b0plot, b1plot, b2plot, ncol = 3)
full_plot
