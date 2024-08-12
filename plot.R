library(ggplot2)

ggplot(mtcars, aes(mpg, wt),col='red') +
  geom_point()

ggsave('plot.png')
