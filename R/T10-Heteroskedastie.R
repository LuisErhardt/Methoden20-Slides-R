library(tidyverse)
library(stargazer)
library(here)
library(ggpubr)
library(icaeDesign)

dgp <- function(x1, x2, beta0, beta1, beta2, hetero=FALSE){
  y <- rep(NA, length(x1))
  sd_hetero <- 6 * x1
  sd_homo <- mean(sd_hetero)
  if (hetero){
    errors <- rnorm(n = length(x1), mean = 0, 
                    sd = sd_hetero)
  } else {
    errors <- rnorm(n = length(x1), mean = 0, 
                    sd = sd_homo
    )
  }
  for (i in 1:length(x1)){
    y[i] <- beta0 + beta1*x1[i] + beta2*x2[i] + errors[i]
  }
  final_data <- tibble(Erfolg=y, Einkommen=x1, Gesundheit=x2, Fehler=errors)
  return(final_data)
}

set.seed("1")
n_stichproben <- 250
n_beobachtungen <- 500
x1_data <- runif(n = n_beobachtungen, min = 1, max = 3)
x2_data <- runif(n = n_beobachtungen, min = 0, max = 1)

wahres_b0 <- 1
wahres_b1 <- 2
wahres_b2 <- 3

full_data_homo <- dgp(x1_data, x2_data, wahres_b0, wahres_b1, wahres_b2, hetero = F)
full_data_hetero <- dgp(x1_data, x2_data, wahres_b0, wahres_b1, wahres_b2, hetero = T)

schaetzung_homo <- lm(Erfolg~Einkommen+Gesundheit, data = full_data_homo)
schaetzung_hetero <- lm(Erfolg~Einkommen+Gesundheit, data = full_data_hetero)

stargazer(
  schaetzung_homo, schaetzung_hetero, 
  omit.stat = c("f", "adj.rsq"), 
  float = F, style = "ajps", 
  out=here("figures/T10/heteroskedastie.tex")
)

homo_plot <- ggplot(data = full_data_homo, aes(x=Einkommen, y=Erfolg)) +
  labs(title = "Fall von Homoskedastie") +
  geom_point(size=0.25, alpha=0.25) + theme_icae()

hetero_plot <- ggplot(data = full_data_hetero, aes(x=Einkommen, y=Erfolg)) +
  labs(title = "Fall von Heteroskedastie") +
  geom_point(size=0.25, alpha=0.25) + theme_icae()

heterosk_plot <- ggarrange(homo_plot, hetero_plot, ncol = 2)

ggsave(plot = heterosk_plot,
       filename = here("figures/T10/hetero1.pdf"), 
       width = 5, height = 3)
