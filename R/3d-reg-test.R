# Beispiel mit marketing_data
library(tidyverse)
library(datarium)
library(plot3D)
library(here)

marketing_data <- datarium::marketing
head(marketing_data)

x = marketing_data$facebook
y = marketing_data$newspaper
z = marketing_data$sales
fit <- lm(z ~ x + y)
grid.lines = 25
x.pred     = seq(min(x), max(x), length.out = grid.lines)
y.pred     = seq(min(y), max(y), length.out = grid.lines)
xy         = expand.grid(x = x.pred, y = y.pred)
z.pred = matrix(predict(fit, newdata = xy), 
                nrow = grid.lines, ncol = grid.lines)
fitpoints = predict(fit)

cols <- viridis::viridis(2)

pdf(file = here("figures/T5/3d-example.pdf"), 
    height = 6, 
    width = 6)

scatter3D(x, y, z, 
          pch = 3, cex = 1, 
          col = cols[1], alpha=0.5, lighting = TRUE,
          theta = -40, phi = 0, # viewing angles
          #ticktype = "detailed",
          xlab = "Facebookwerbung", ylab = "Zeitungswerbung", zlab = "Verkäufe", 
          zlim = c(0, 40), clim = c(0, 40))
dev.off()

pdf(file = here("figures/T5/3d-example-reg.pdf"), 
    height = 6, 
    width = 6)

scatter3D(x, y, z, 
          pch = 3, cex = 1, 
          col = cols[1], alpha=0.5, lighting = TRUE,
          theta = -40, phi = 0, # viewing angles
          #ticktype = "detailed",
          xlab = "Facebookwerbung", ylab = "Zeitungswerbung", zlab = "Verkäufe", 
          zlim = c(0, 40), clim = c(0, 40),
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, col=cols[2]))
dev.off()

