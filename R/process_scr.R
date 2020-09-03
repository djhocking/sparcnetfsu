

#---------- Setup ----------
# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(here)
library(tidyr)
library(oSCR)

#read data
sally <- read_csv(here("analysis", "data", "raw_data", "salamander.csv"))

# Set board grid
boards <- expand.grid(col = LETTERS[1:5],
                      row = 0:9,
                      stringsAsFactors = FALSE) %>%
  arrange(col, row) %>%
  tidyr::unite("board", col:row, sep = "") %>%
  mutate(X = rep(0:4, each = 10),
         Y = rep(0:9, 5))

#--------- Clean and Prep Data ---------
sally <- sally %>%
  dplyr::mutate(date = mdy(date),
                doy = yday(date),
                doy_s = (doy - 182.5) / 182.5,
                doy_s_2 = doy_s ^ 2) %>%
  dplyr::arrange(plot, date, board)

ggplot(filter(sally, species == "PCIN"), aes(svl, mass)) + geom_point(alpha = 0.3, color = "gray30") + theme_bw()

# make ss and days since last primary period
for(i in 1:length(unique(sally$plot))) {
# not needed yet
}

# pull out just data for 1 plot with valid ids
pcin_1 <- sally %>%
  dplyr::filter(species == "PCIN",
                plot == 5,
                !is.na(id),
                !(id %in% c("<NA>"))) %>%
  # tidyr::unite("id", c(sex, morph, markf), remove = FALSE) %>%
  dplyr::select(pp, id, ss, board, sex) %>%
  filter(sex %in% c("M", "F")) %>%
  dplyr::mutate(id = as.factor(id),
                sex = as.factor(if_else(sex %in% c("M", "F"), sex, "U"))) %>% # oSCR can only handle 2 sexes
  as.data.frame()

# construct tdf
doy <- pcin_1 %>%
  group_by(pp) %>%
  dplyr::select(pp, ss, doy_s, day_s_2)

#--------- Check data ----------
summary(pcin_1)

# check that boards properly recorded
unique(pcin_1$board)

# Check for duplicate IDs
counts <- pcin_1 %>%
  # select(-Row, -flag) %>%
  filter(!is.na(id)) %>%
  # distinct() %>%
  group_by(id, pp, ss) %>%
  select(id, pp, ss) %>%
  summarise(count = n()) %>%
  filter(count > 1)

counts

pcin_wide <- pcin_1 %>%
  # select(-Row, -flag) %>%
  filter(!is.na(id)) %>%
  # distinct() %>%
  group_by(id, pp, ss) %>%
  select(id, pp, ss) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(id) %>%
  tidyr::pivot_wider(names_from = c("pp", "ss"), values_from = count, names_sort = TRUE, names_prefix = "x")
pcin_wide

# more duplicate checking
wide_summary <- pcin_wide %>%
  ungroup() %>%
  mutate(max = apply(select(., starts_with("x")), 1, max, na.rm = TRUE),
         sum = rowSums(select(., starts_with("x")), na.rm = TRUE)) %>%
  dplyr::select(id, max, sum) %>%
  as.data.frame()

wide_summary
summary(wide_summary)

# check if any sessions with no captures
pcin_wide %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  colSums(. , na.rm = TRUE)

# even more duplicate checking
length(unique(pcin_1$id)) == nrow(pcin_wide)


# how many occasions (ss) per session (pp)
sess_obs <- sally %>%
  group_by(pp) %>%
  dplyr::select(pp, ss) %>%
  summarise(K = max(ss, na.rm = TRUE)) %>%
  arrange(pp)

sess_obs_1 <- pcin_1 %>%
  group_by(pp) %>%
  dplyr::select(pp, ss) %>%
  summarise(K = max(ss, na.rm = TRUE)) %>%
  arrange(pp)

sess_obs == sess_obs_1

# Check sex for consistency by mark (this is the problem for plot 1)


#-------- Set up oSCR data ---------

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

#---------- SCR Analysis ---------

m16 <- oSCR.fit(model = list(D ~ pp, #density
                             p0 ~ pp + sex, #detection
                             sig ~ pp + sex), #space use
                scrFrame = pcin_1_ss, ssDF = pcin_1_oscr$scrFrame)
