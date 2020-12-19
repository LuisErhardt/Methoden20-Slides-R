library(here)
library(stargazer)

# Schuhgroesse und Geschlecht--------------------------------------------------
b0_schuh <- 40
b1_schuh <- 1.5
b0_einkommen <- 500
b1_einkommen <- 50
nobs <- 100
male <- as.integer(nobs/2)
female <- nobs - male

set.seed(123)

Geschlecht <- c(rep(0, female), rep(1, male))
Schuhgroesse <- b0_schuh + b1_schuh*Geschlecht + rnorm(nobs) 
Einkommen <- b0_einkommen + b1_einkommen*Geschlecht + rnorm(nobs) 

schuhset <- tibble(
  Geschlecht=Geschlecht,
  Schuhgroesse=Schuhgroesse,
  Einkommen=Einkommen
)

lm_schuh <- lm(Einkommen~Schuhgroesse, data = schuhset)
lm_full <- lm(Einkommen~Schuhgroesse+Geschlecht, data = schuhset)

summary(lm_schuh)
summary(lm_full)

stargazer(lm_schuh, lm_full, 
          omit.stat = c("f", "adj.rsq"), 
          float = F, style = "ajps", 
          out=here("figures/T10/omitted_regs.tex")
          )


# Schuhgroesse, Intelligenz und Geschlecht-------------------------------------
b0_schuh <- 40
b1_schuh <- 1.5
b0_einkommen <- 500
b1_einkommen <- 50
b2_einkommen <- 40

nobs <- 100
male <- as.integer(nobs/2)
female <- nobs - male

set.seed(123)

Geschlecht <- c(rep(0, female), rep(1, male))
Intelligenz <- rnorm(n = nobs, mean = 50, sd = 2)
Schuhgroesse <- b0_schuh + b1_schuh*Geschlecht + rnorm(nobs) 
Einkommen <- b0_einkommen + b1_einkommen*Geschlecht + 
  b2_einkommen*Intelligenz + rnorm(nobs) 

schlauset <- tibble(
  Geschlecht=Geschlecht,
  Intelligenz=Intelligenz,
  Schuhgroesse=Schuhgroesse,
  Einkommen=Einkommen
)

lm_lol <- lm(Einkommen~Schuhgroesse, data = schlauset)
lm_dumm <- lm(Einkommen~Schuhgroesse+Geschlecht, data = schlauset)
lm_schlau <- lm(Einkommen~Schuhgroesse+Intelligenz+Geschlecht, data = schlauset)

summary(lm_dumm)
summary(lm_schlau)

stargazer(
  lm_lol, lm_dumm, lm_schlau, 
  omit.stat = c("f", "adj.rsq"), 
  float = F, style = "ajps", 
  out=here("figures/T10/omitted_regs_dumm.tex")
  )
