# This is the introduction to ggplot2
# Author: Christopher Peters
# Email: statwonk@gmail.com

# install.packages("ggplot2")
library(ggplot2)

str(iris)

ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_histogram()