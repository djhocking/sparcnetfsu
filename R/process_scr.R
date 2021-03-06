

#---------- Setup ----------
# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(here)
library(tidyr)
library(oSCR)

# testing
testing <- TRUE

#read data
sally <- read_csv(here("analysis", "data", "raw_data", "salamander.csv"))

# Make board grid
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


# temperature data
temp <- read_csv(here::here("analysis", "data", "raw_data", "environmental_data.csv")) %>%
  rename_all(tolower) %>%
  mutate(soil = rowMeans(dplyr::select(., starts_with("soil")), na.rm = TRUE),
         air_s = (air - mean(air, na.rm = TRUE)) / sd(air, na.rm = TRUE),
         soil_s = (soil - mean(soil, na.rm = TRUE)) / sd(soil, na.rm = TRUE))

temp <- sally %>%
  dplyr::select(date, plot, pp, ss) %>%
  distinct() %>%
  left_join(temp) %>%
  group_by(pp, ss) %>%
  dplyr::select(-date, -plot) %>%
  dplyr::summarise_all(.funs = mean, na.rm = TRUE) %>%
  arrange(pp, ss)

temp_s <- temp %>%
  ungroup() %>%
  group_by(pp) %>%
  dplyr::select(pp, ss, air_s, soil_s) %>%
  group_split()

# moisture data
moist <- read_csv(here::here("analysis", "data", "raw_data", "soil_moisture.csv")) %>%
  rename_all(tolower)

moist <- sally %>%
  dplyr::select(date, plot, pp, ss) %>%
  distinct() %>%
  left_join(moist) %>%
  arrange(date, plot, pp, ss)

moist %>%
  dplyr::filter(is.na(water_pct)) %>%
  as.data.frame() # too much missing to deal with now. Use rain instead

# rain
rain <- read_csv(here::here("analysis", "data", "raw_data", "weather_station.csv")) %>%
  rename_all(tolower) %>%
  dplyr::select(-month, -day, -doy, -year, -remarks) %>%
  dplyr::mutate(date = ymd(date),
                rain = raineq_in * 2.54,
                rain_3d = RcppRoll::roll_sum(rain, 3, align = "right", fill = NA),
                rain_3d_s = (rain_3d - mean(rain_3d, na.rm = TRUE)) / sd(rain_3d, na.rm = TRUE))

rain <- sally %>%
  dplyr::select(date, plot, pp, ss) %>%
  distinct() %>%
  left_join(rain) %>%
  group_by(pp, ss) %>%
  dplyr::select(-date, -plot, -snowpack) %>%
  dplyr::summarise_all(.funs = mean, na.rm = TRUE) %>%
  arrange(pp, ss)

rain_s <- rain %>%
  ungroup() %>%
  group_by(pp) %>%
  dplyr::select(pp, ss, rain_3d_s) %>%
  group_split()

# construct tdf
doy <- sally %>%
  group_by(pp, ss) %>%
  dplyr::select(pp, ss, doy_s, doy_s_2) %>%
  summarise_all(.funs = mean, na.rm = TRUE)

doys <- doy %>%
  ungroup() %>%
  group_by(pp) %>%
  dplyr::select(pp, ss, doy_s) %>%
  group_split()

doys2 <- doy %>%
  ungroup() %>%
  group_by(pp) %>%
  dplyr::select(pp, ss, doy_s_2) %>%
  group_split()

tdf <- list()
for(i in 1:length(unique(sally$pp))) {
  tmp1 <- doys[[i]] %>%
    tidyr::pivot_wider(names_from = ss, values_from = doy_s, names_prefix = "doys.") %>%
    dplyr::select(-pp) %>%
    data.frame(boards, sep = "/", .)
  tmp2 <- doys2[[i]] %>%
    tidyr::pivot_wider(names_from = ss, values_from = doy_s_2, names_prefix = "doys2.") %>%
    dplyr::select(-pp) %>%
    data.frame(boards, sep = "/", .)
  tmp3 <- rain_s[[i]] %>%
    tidyr::pivot_wider(names_from = ss, values_from = rain_3d_s, names_prefix = "rain.") %>%
    dplyr::select(-pp) %>%
    data.frame(boards, sep = "/", .)
  tmp4 <- temp_s[[i]] %>%
    dplyr::select(-soil_s) %>%
    tidyr::pivot_wider(names_from = ss, values_from = air_s, names_prefix = "air.") %>%
    dplyr::select(-pp) %>%
    data.frame(boards, sep = "/", .)
  tmp5 <- temp_s[[i]] %>%
    dplyr::select(-air_s) %>%
    tidyr::pivot_wider(names_from = ss, values_from = soil_s, names_prefix = "soil.") %>%
    dplyr::select(-pp) %>%
    data.frame(boards, sep = "/", .)
  tdf[[i]] <- tmp1 %>%
    left_join(tmp2) %>%
    left_join(tmp3) %>%
    left_join(tmp4) %>%
    left_join(tmp5)
}

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
                         tdf = list(tdf[[1]],
                                    tdf[[2]],
                                    tdf[[3]],
                                    tdf[[4]],
                                    tdf[[5]],
                                    tdf[[6]]),
                         K = sess_obs$K,
                         ntraps = rep(50, length(sess_obs$K)),
                         trapcov.names = c("doys", "doys2", "rain", "air", "soil"),
                         tdf.sep = "/",
                         sex.nacode = "U")

names(pcin_1_oscr)
names(pcin_1_oscr$scrFrame)
pcin_1_oscr$scrFrame

par(mfrow = c(3, 2), mar = c(2, 2, 2, 2), oma = c(0, 0, 0, 0))
plot(pcin_1_oscr$scrFrame, jit = 2)

if(testing) {
pcin_1_ss <- make.ssDF(scrFrame = pcin_1_oscr$scrFrame, buffer = 1, res = 1)
} else {
  pcin_1_ss <- make.ssDF(scrFrame = pcin_1_oscr$scrFrame, buffer = 4, res = 0.5)
}
plot(ssDF = pcin_1_ss, scrFrame = pcin_1_oscr$scrFrame)

#---------- SCR Analysis ---------

m16 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ b + session + sex, #detection - fails with behavioral model for just one plot
                             sig ~ 1 + sex), #space use
                scrFrame = pcin_1_oscr$scrFrame, ssDF = pcin_1_ss)

m0 <- oSCR.fit(model = list(D ~ 1, #density
                             p0 ~ 1, #detection
                             sig ~ 1), #space use
                scrFrame = pcin_1_oscr$scrFrame, ssDF = pcin_1_ss)

m16 <- oSCR.fit(model = list(D ~ 1, #density
                             p0 ~ air * rain, #detection - fails with behavioral model for just one plot
                             sig ~ 1), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss,
                se = TRUE)


m16 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ session + sex, #detection - fails with behavioral model for just one plot
                             sig ~ 1 + sex), #space use
                scrFrame = pcin_1_oscr$scrFrame, ssDF = pcin_1_ss)



