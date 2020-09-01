library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(here)
library(oSCR)

sally <- read_csv(here("analysis", "data", "raw_data", "salamander.csv"))

sally <- sally %>%
  dplyr::mutate(date = mdy(date)) %>%
  dplyr::arrange(plot, date, board)

ggplot(filter(sally, species == "PCIN"), aes(svl, mass)) + geom_point(alpha = 0.3, color = "gray30") + theme_bw()

# make ss and days since last primary period
for(i in 1:length(unique(sally$plot))) {

}

pcin_1 <- sally %>%
  dplyr::filter(species == "PCIN",
                plot == 1,
                !is.na(markf),
                !(markf %in% c("<NA>"))) %>%
  tidyr::unite("id", c(sex, morph, markf), remove = FALSE) %>%
  dplyr::select(pp, id = id, ss, board, sex) %>%
  dplyr::mutate(id = as.factor(id),
                sex = as.factor(if_else(sex %in% c("M", "F"), sex, "U"))) %>% # oSCR can only handle 2 sexes
  as.data.frame()

# ?data2oscr

# how many occasions (ss) per session (pp)
sess_obs <- sally %>%
  group_by(pp) %>%
  dplyr::select(pp, ss) %>%
  summarise(K = max(ss, na.rm = TRUE)) %>%
  arrange(pp)

boards <- expand.grid(col = LETTERS[1:5],
                      row = 0:9,
                      stringsAsFactors = FALSE) %>%
  arrange(col, row) %>%
  tidyr::unite("board", col:row, sep = "") %>%
  mutate(X = rep(0:4, each = 10),
         Y = rep(0:9, 5))

pcin_1_oscr <- data2oscr(edf = pcin_1,
                         sess.col = 1,
                         id.col = 2,
                         occ.col = 3,
                         trap.col = 4,
                         sex.col = 5,
                         tdf = list(boards,
                                    boards,
                                    boards,
                                    boards,
                                    boards,
                                    boards),
                         K = sess_obs$K,
                         ntraps = rep(50, length(sess_obs$K)),
                         sex.nacode = "U")

names(pcin_1_oscr)
names(pcin_1_oscr$scrFrame)
pcin_1_oscr$scrFrame

par(mfrow = c(3, 2), mar = c(2, 2, 2, 2), oma = c(0, 0, 0, 0))
plot(pcin_1_oscr$scrFrame, jit = 2)

pcin_1_ss <- make.ssDF(scrFrame = pcin_1_oscr$scrFrame, buffer = 4, res=0.5)
plot(ssDF = pcin_1_ss, scrFrame = pcin_1_oscr$scrFrame)


m16 <- oSCR.fit(model = list(D ~ pp, #density
                             p0 ~ pp + sex, #detection
                             sig ~ pp + sex), #space use
                scrFrame = pcin_1_ss, ssDF = pcin_1_oscr$scrFrame)
