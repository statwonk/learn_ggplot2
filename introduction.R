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

chris_theme <- function(){ # this will likely be common across your 
  # graphs in a single article, so why not drop it in a function and
  # make it nice and simple to call?
  theme_few() +
    theme(axis.text = element_text(size = 16),
          axis.text.x = element_text(colour = "red"),
          axis.title = element_text(size = 18),
          axis.title.y = element_text(vjust = 0.2),
          axis.title.x = element_text(vjust = -0.2),
          strip.text = element_text(size = 20, colour = "blue"))
}

ggplot(data = iris, aes(x = Sepal.Length)) + 
  geom_histogram() + # this is a simple example of a histogram
  facet_wrap( ~ Species) +
  ggtitle("Iris Dataset, learning ggplot!") +
  ylab("Frequency") +
  xlab("Sepal Length (units = cm)") +
  chris_theme()

ggsave(p, "iris.png", height = 8, width = 11, units = "in",
       dpi = 200)

# You can assign the ggplot to a variable and call it in ggsave
p <- ggplot(data = iris, aes(x = Sepal.Length)) + 
  geom_histogram() + # this is a simple example of a histogram
  facet_wrap( ~ Species) +
  ggtitle("Iris Dataset, learning ggplot!") +
  ylab("Frequency") +
  xlab("Sepal Length (units = cm)") +
  chris_theme()
ggsave(p, "iris.png", height = 8, width = 11, units = "in",
       dpi = 200)   




