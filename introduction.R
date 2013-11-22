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
ggsave("iris.png", plot = p, height = 8, width = 11, units = "in",
       dpi = 200)   

mtc

days <- seq(as.POSIXct("2013-01-01", tz = "UTC"), 
            as.POSIXct("2013-12-31", tz = "UTC"),
            by = "day")
data <- rep(NA, 365)
for(i in 1:365){
  set.seed(i)
  data[i] <- rnorm(1, 1, 1)
}
df <- data.frame(list(date = days, values = data))

# install.packages("scales")
library(scales)

ggplot(df, aes(x = date, y = values)) +
  geom_histogram(stat = "identity") +
  scale_x_datetime(breaks = date_breaks("3 months"))

install.packages("dismo")
library(dismo)

gbm.plot
chris_gbm_plot <- function (gbm.object, variable.no = 0, smooth = FALSE, rug = TRUE, 
          n.plots = length(pred.names), common.scale = TRUE, write.title = TRUE, 
          y.label = "fitted function", x.label = NULL, show.contrib = TRUE, 
          plot.layout = c(3, 4), ...) 
{
  if (!require(gbm)) {
    stop("you need to install the gbm package to run this function")
  }
  if (!require(splines)) {
    stop("you need to install the splines package to run this function")
  }
  gbm.call <- gbm.object$gbm.call
  gbm.x <- gbm.call$gbm.x
  pred.names <- gbm.call$predictor.names
  response.name <- gbm.call$response.name
  dataframe.name <- gbm.call$dataframe
  data <- eval(parse(text = dataframe.name))
  max.plots <- plot.layout[1] * plot.layout[2]
  plot.count <- 0
  n.pages <- 1
  if (length(variable.no) > 1) {
    stop("only one response variable can be plotted at a time")
  }
  if (variable.no > 0) {
    n.plots <- 1
  }
  max.vars <- length(gbm.object$contributions$var)
  if (n.plots > max.vars) {
    n.plots <- max.vars
    warning("reducing no of plotted predictors to maximum available (", 
            max.vars, ")")
  }
  predictors <- list(rep(NA, n.plots))
  responses <- list(rep(NA, n.plots))
  for (j in c(1:n.plots)) {
    if (n.plots == 1) {
      k <- variable.no
    }
    else k <- match(gbm.object$contributions$var[j], pred.names)
    if (is.null(x.label)) 
      var.name <- gbm.call$predictor.names[k]
    else var.name <- x.label
    pred.data <- data[, gbm.call$gbm.x[k]]
    response.matrix <- plot.gbm(gbm.object, k, return.grid = TRUE)
    predictors[[j]] <- response.matrix[, 1]
    if (is.factor(data[, gbm.call$gbm.x[k]])) {
      predictors[[j]] <- factor(predictors[[j]], levels = levels(data[, 
                                                                      gbm.call$gbm.x[k]]))
    }
    responses[[j]] <- response.matrix[, 2] - mean(response.matrix[, 
                                                                  2])
    if (j == 1) {
      ymin = min(responses[[j]])
      ymax = max(responses[[j]])
    }
    else {
      ymin = min(ymin, min(responses[[j]]))
      ymax = max(ymax, max(responses[[j]]))
    }
  }
  op <- par(no.readonly = TRUE)
  par(mfrow = plot.layout)
  for (j in c(1:n.plots)) {
    if (plot.count == max.plots) {
      plot.count = 0
      n.pages <- n.pages + 1
    }
    plot.count <- plot.count + 1
    if (n.plots == 1) {
      k <- match(pred.names[variable.no], gbm.object$contributions$var)
      if (show.contrib) {
        x.label <- paste(var.name, "  (", round(gbm.object$contributions[k, 
                                                                         2], 1), "%)", sep = "")
      }
    }
    else {
      k <- match(gbm.object$contributions$var[j], pred.names)
      var.name <- gbm.call$predictor.names[k]
      if (show.contrib) {
        x.label <- paste(var.name, "  (", round(gbm.object$contributions[j, 
                                                                         2], 1), "%)", sep = "")
      }
      else x.label <- var.name
    }
    if (common.scale) {
      plot(predictors[[j]], responses[[j]], ylim = c(ymin, 
                                                     ymax), type = "l", xlab = x.label, ylab = y.label, 
           ...)
    }
    else {
      plot(predictors[[j]], responses[[j]], type = "l", 
           xlab = x.label, ylab = y.label, ...)
    }
    if (smooth & is.vector(predictors[[j]])) {
      temp.lo <- loess(responses[[j]] ~ predictors[[j]], 
                       span = 0.3)
      lines(predictors[[j]], fitted(temp.lo), lty = 2, 
            col = 2)
    }
    if (plot.count == 1) {
      if (write.title) {
        title(paste(response.name, " - page ", n.pages, 
                    sep = ""))
      }
      if (rug & is.vector(data[, gbm.call$gbm.x[variable.no]])) {
        rug(quantile(data[, gbm.call$gbm.x[variable.no]], 
                     probs = seq(0, 1, 0.1), na.rm = TRUE))
      }
    }
    else {
      if (write.title & j == 1) {
        title(response.name)
      }
      if (rug & is.vector(data[, gbm.call$gbm.x[k]])) {
        rug(quantile(data[, gbm.call$gbm.x[k]], probs = seq(0, 
                                                            1, 0.1), na.rm = TRUE))
      }
    }
  }
  par(op)
}



