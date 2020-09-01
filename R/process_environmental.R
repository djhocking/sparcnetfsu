
library(readr)
library(dplyr)
library(lubridate)
library(stringr)


files <- list.files("analysis/data/raw_data/temperature/air")
columns <- c("row", "datetime", "temp", "lux", "bad", "good", "coupler", "host", "stopped", "end")

###########
# need to clip raw data to days in the field and not include time in lab before downloading
###########

# air <- list()
for(i in 1:length(files)) {
  tmp <- read_csv(file.path("analysis/data/raw_data/temperature/air", files[i]), skip = 2, col_names = FALSE)
  tmp_names <- columns[1:ncol(tmp)]
  names(tmp) <- tmp_names
  if(i == 1) {
    df_air <- tmp
  } else {
    df_air <- bind_rows(df_air, tmp)
  }
}

df_air <- df_air %>%
  mutate(datetime = parse_date_time(datetime, orders = c("%m/%d/%y %H:%M", "%m/%d/%y %I:%M:%S %p")))

summary(df_air)
unique(df_air$bad)
