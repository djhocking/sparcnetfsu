library(stringr)

# get list of files to parse
files <- list.files("analysis/data/raw_data/temperature/air", include.dirs = TRUE, full.names = TRUE)

plots <- rep(NA_integer_, length(files))
for(i in 1:length(files)) {
  plots[i] <- as.integer(str_extract(read.table(files[i], nrows = 1, sep = ",")[1, 1], "[0-9]"))
}


