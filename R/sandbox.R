# load libraries for use in script
library(ggplot2)
library(dplyr)

# plot petal measurments of built-in iris data
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()

# load in salamander data
sally <- read.csv("analysis/data/raw_data/salamander.csv", stringsAsFactors = FALSE)

# plot svl vs. mass
ggplot(data = sally, aes(svl, mass)) + geom_point() + geom_smooth()

# check unique species codes
unique(sally$species)

# filter data to just red-backed salamanders
pcin <- sally %>%
  dplyr::filter(species == "PCIN")

# Plot just redbacks
ggplot(pcin, aes(svl, mass)) + geom_point() + geom_smooth()
