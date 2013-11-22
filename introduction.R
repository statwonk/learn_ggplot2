# This is the introduction to ggplot2
# Author: Christopher Peters
# Email: statwonk@gmail.com

# install.packages("ggplot2")
library(ggplot2)

str(iris)

ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_histogram() # this is a simple example of a histogram

# install.packages("ggthemes")
library(ggthemes)

ggplot(data = iris, aes(x = Sepal.Length)) + 
  geom_histogram() + # this is a simple example of a histogram
  facet_wrap( ~ Species) +
  ggtitle("Iris Dataset, learning ggplot!") +
  ylab("Frequency") +
  xlab("Sepal Length (units = cm)") +
  chris_theme()
  
chris_theme <- function(){ # this will likely be common across your 
  # graphs in a single article, so why not drop it in a function and
  # make it nice and simple to call?
  theme_wsj() +
  theme(axis.text = element_text(size = 16),
        axis.text.x = element_text(colour = "red"),
        strip.text = element_text(size = 20, colour = "blue"))
}



