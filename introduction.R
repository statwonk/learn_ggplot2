# This is the introduction to ggplot2
# Author: Christopher Peters
# Email: statwonk@gmail.com

# install.packages("ggplot2")
library(ggplot2)

str(iris)

ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_histogram() # this is a simple example of a histogram

ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_histogram() + # this is a simple example of a histogram
  facet_wrap( ~ Species) +
  ggtitle("Iris Dataset, learning ggplot!")

