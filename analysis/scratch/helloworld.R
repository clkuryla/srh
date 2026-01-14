library(ggplot2)
# Example plot using the built-in 'mtcars' dataset
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = "Scatter Plot of mpg vs wt")
