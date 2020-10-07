library("plot3D")
library(here)

autompg = read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
  quote = "\"",
  comment.char = "",
  stringsAsFactors = FALSE)
# give the dataframe headers
colnames(autompg) = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin", "name")
# remove missing data, which is stored as "?"
autompg = subset(autompg, autompg$hp != "?")
# remove the plymouth reliant, as it causes some issues
autompg = subset(autompg, autompg$name != "plymouth reliant")
# give the dataset row names, based on the engine, year and name
rownames(autompg) = paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
# remove the variable for name, as well as origin
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year"))
# change horsepower from character to numeric
autompg$hp = as.numeric(autompg$hp)
# check final structure of data
str(autompg)


x = autompg$wt
y = autompg$year
z = autompg$mpg
fit <- lm(z ~ x + y)
grid.lines = 25
x.pred     = seq(min(x), max(x), length.out = grid.lines)
y.pred     = seq(min(y), max(y), length.out = grid.lines)
xy         = expand.grid(x = x.pred, y = y.pred)
z.pred = matrix(predict(fit, newdata = xy), 
                nrow = grid.lines, ncol = grid.lines)
fitpoints = predict(fit)

cols <- viridis::viridis(2)

pdf(file = here("3d-example.pdf"), 
    height = 6, 
    width = 6)

scatter3D(x, y, z, 
          pch = 3, cex = 1, 
          col = cols[1], alpha=0.5, lighting = TRUE,
          theta = 145, phi = 5, # viewing angles
          #ticktype = "detailed",
          xlab = "wt", ylab = "year", zlab = "mpg", 
          zlim = c(0, 40), clim = c(0, 40),
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, #fit = fitpoints,
                      col=cols[2]))
dev.off()

