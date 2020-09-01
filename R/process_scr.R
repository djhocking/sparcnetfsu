library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

sally <- read_csv("analysis/data/raw_data/salamander.csv")

sally <- sally %>%
  dplyr::mutate(date = ymd(date)) %>%
  dplyr::arrange(plot, date, board)

ggplot(filter(sally, species == "PCIN"), aes(svl, mass)) + geom_point(alpha = 0.3, color = "gray30") + theme_bw()

# make ss and days since last primary period
for(i in 1:length(unique(sally$plot))) {

}
