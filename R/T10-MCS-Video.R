# Hinweise: Das ist das Beispiel aus dem Video. Besser ist es, sich die 
# ausfuehrlich kommentierte Variante in T10-MonteCarlo.R anzuschauen!

library(tidyverse)

# 1. Anzahl der Versuche
n_trials <- 10000
# 2. Container definiert
results <- rep(NA, n_trials)

# 3. Seed gesetzt
set.seed(123)

# 4. Durchführung mittels Schleife
for (i in 1:n_trials){
  resulting_nb <- rnorm(n = 1, mean = 0, sd = 1)
  results[i] <- resulting_nb
}

# 5. Visualisisert
ggplot(
  data = tibble(results), mapping = aes(x=results)
) +
  geom_histogram(
    aes(y=..density..),
    alpha = 0.5,
    bins = 50) +
  stat_function(fun = dnorm, args = list(mean=0, sd=1), color="blue") +
  scale_y_continuous(
    expand = expansion(add = c(0, 0.05))
    ) +
  theme_bw()

# 1. Anzahl der Versuche:
n_trials <- 10000
# 2. Container
resultat <- rep(NA, n_trials)
# 3. Seed setzen
set.seed(123)

# 4. MCS durchführen
for (i in 1:n_trials){
  stichprobe <- runif(100)
  mittelwert <- mean(stichprobe)
  resultat[i] <- mittelwert
}

# 5. Visualisieren
ggplot(data = tibble(resultat), 
       mapping = aes(x=resultat)
       ) +
  geom_histogram(
    aes(y=..density..),
    alpha=0.5,
    binwidth = 0.01
  ) +
  scale_y_continuous(
    expand = expansion(add = c(0, 0.05))
    ) +
  theme_bw()

set.seed(123)

simulationen <- 5000
sample_size <- 500

b0 <- 1
b1 <- 2
b2 <- 4

b0_schaetzungen <- rep(NA, simulationen)
b1_schaetzungen <- rep(NA, simulationen)
b2_schaetzungen <- rep(NA, simulationen)

for (i in 1:simulationen){
  # 1. Datensatz erstellen
  x1_var <- runif(n = sample_size, min = 2, max = 5)
  x2_var <- runif(n = sample_size, min = 5, max = 10)
  fehler <- rnorm(n = sample_size, mean = 0, sd = 100)
  y_var <- b0 + b1*x1_var + b2*x2_var + fehler
  
  datensatz <- tibble(
    x1 = x1_var, 
    x2 = x2_var,
    yvar = y_var
  )
  
  # 2. Schaetzung
  model_est <- lm(yvar~x1+x2, data = datensatz)
  
  # 3. Geschätzen Werte speichern
  b0_schaetzungen[i] <- unname(model_est[["coefficients"]]["(Intercept)"])
  b1_schaetzungen[i] <- unname(model_est[["coefficients"]]["x1"])
  b2_schaetzungen[i] <- unname(model_est[["coefficients"]]["x2"])
}

ggplot(data = tibble(b1_schaetzungen), mapping = aes(x=b1_schaetzungen)) +
  geom_histogram(
    aes(y=..density..), alpha=0.5, binwidth = 0.5) +
  geom_vline(xintercept = b1) +
  scale_y_continuous(
    expand = expansion(add = c(0, 0.05))) +
  theme_bw()

ggplot(data = tibble(b2_schaetzungen), mapping = aes(x=b2_schaetzungen)) +
  geom_histogram(
    aes(y=..density..), alpha=0.5, binwidth = 0.01) +
  geom_vline(xintercept = b2) +
  scale_y_continuous(
    expand = expansion(add = c(0, 0.05))) +
  theme_bw()
