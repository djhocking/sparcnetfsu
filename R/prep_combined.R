

#---------- Setup ----------
# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(here)
library(tidyr)
# library(oSCR)

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
  dplyr::filter(!is.na(id),
                !is.na(plot),
                !is.na(pp),
                !is.na(ss),
                board %in% unique(boards$board)) %>%
  dplyr::mutate(date = mdy(date),
                session = as.character(paste0("p", plot, "-", pp)),
                id = paste0(plot, "-", id, "-", sex),
                doy = yday(date),
                doy_s = (doy - 182.5) / 182.5,
                doy_s_2 = doy_s ^ 2) %>%
  dplyr::arrange(plot, date, board)

# ggplot(filter(sally, species == "PCIN"), aes(svl, mass)) + geom_point(alpha = 0.3, color = "gray30") + theme_bw()

# make ss and days since last primary period
for(i in 1:length(unique(sally$plot))) {
  # not needed yet
}

# pull out just data for 1 plot with valid ids
pcin_1 <- sally %>%
  dplyr::filter(species == "PCIN",
                # plot == 5,
                !is.na(id),
                !(id %in% c("<NA>"))) %>%
  # tidyr::unite("id", c(sex, morph, markf), remove = FALSE) %>%
  dplyr::select(session, id, ss, board, sex) %>%
  # filter(sex %in% c("M", "F")) %>%
  dplyr::mutate(id = as.factor(id),
                sex = as.factor(if_else(sex %in% c("M", "F"), sex, "U"))) %>% # oSCR can only handle 2 sexes
  arrange(session, ss, id) %>%
  as.data.frame()

# temperature data
temp <- read_csv(here::here("analysis", "data", "raw_data", "environmental_data.csv")) %>%
  rename_all(tolower) %>%
  mutate(r24 = as.logical(r24),
         soil = rowMeans(dplyr::select(., starts_with("soil")), na.rm = TRUE),
         air_s = (air - mean(air, na.rm = TRUE)) / sd(air, na.rm = TRUE),
         soil_s = (soil - mean(soil, na.rm = TRUE)) / sd(soil, na.rm = TRUE))

# sessions
sess_obs <- sally %>%
  group_by(session) %>%
  dplyr::select(session, ss) %>%
  summarise(K = max(ss, na.rm = TRUE)) %>%
  arrange(session) %>%
  as.data.frame()

sessions <- expand.grid(plot = 1:6, pp = 1:6) %>%
  dplyr::mutate(session = paste0("p", plot, "-", pp))

pp_obs <- sally %>%
  group_by(pp) %>%
  dplyr::select(pp, ss) %>%
  summarise(K = max(ss, na.rm = TRUE)) %>%
  arrange(pp) %>%
  as.data.frame() %>%
  right_join(sessions) %>%
  arrange(session)

for(i in 1:nrow(pp_obs)) {
  foo <- data.frame(session = pp_obs$session[i],
                    ss = 1:pp_obs$K[i])
  if(i == 1) {
    session_df <- foo
  } else {
    session_df <- bind_rows(session_df, foo)
  }
}

session_df <- session_df %>%
  separate(col = session, sep = "[^[:alnum:]]+", into = c("plot", "pp"), remove = FALSE) %>%
  dplyr::mutate(plot = as.numeric(stringr::str_remove(plot, pattern = "p")),
                pp = as.numeric(pp))

temp <- session_df %>%
  left_join(temp) %>%
  group_by(session, pp, ss) %>%
  dplyr::select(-date, -plot, -site, -observers, -notes) %>%
  dplyr::summarise_all(.funs = mean, na.rm = TRUE) %>%
  distinct() %>%
  arrange(session, pp, ss)

temp_s <- temp %>%
  ungroup() %>%
  group_by(session) %>%
  dplyr::select(session, ss, air_s, soil_s) %>%
  group_split()

# moisture data
moist <- read_csv(here::here("analysis", "data", "raw_data", "soil_moisture.csv")) %>%
  rename_all(tolower)

moist <- session_df %>%
  distinct() %>%
  left_join(moist) %>%
  arrange(date, session, plot, pp, ss)

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
  dplyr::select(date, pp, ss) %>%
  distinct() %>%
  left_join(rain) %>%
  group_by(pp, ss) %>%
  dplyr::select(-date, -snowpack) %>%
  dplyr::summarise_all(.funs = mean, na.rm = TRUE) %>%
  right_join(session_df) %>%
  distinct() %>%
  arrange(session, pp, ss)

rain_s <- rain %>%
  ungroup() %>%
  group_by(session) %>%
  dplyr::select(session, ss, rain_3d_s) %>%
  group_split()

# get stardization parameters
stds <- data.frame(var = c("rain", "soil"), mean = c(mean(rain$rain_3d, na.rm = TRUE), mean(temp$soil, na.rm = TRUE)), sd = c(sd(rain$rain_3d, na.rm = TRUE), sd(temp$soil, na.rm = TRUE)))

# construct tdf
doy <- sally %>%
  group_by(pp, ss) %>%
  dplyr::select(pp, ss, doy_s, doy_s_2) %>%
  summarise_all(.funs = mean, na.rm = TRUE) %>%
  right_join(session_df) %>%
  distinct()

doys <- doy %>%
  ungroup() %>%
  group_by(session) %>%
  dplyr::select(session, ss, doy_s) %>%
  group_split()

doys2 <- doy %>%
  ungroup() %>%
  group_by(session) %>%
  dplyr::select(session, ss, doy_s_2) %>%
  group_split()

tdf <- list()
for(i in 1:length(unique(sally$session))) {
  tmp1 <- doys[[i]] %>%
    tidyr::pivot_wider(names_from = ss, values_from = doy_s, names_prefix = "doys.") %>%
    dplyr::select(-session) %>%
    data.frame(boards, sep = "/", .)
  tmp2 <- doys2[[i]] %>%
    tidyr::pivot_wider(names_from = ss, values_from = doy_s_2, names_prefix = "doys2.") %>%
    dplyr::select(-session) %>%
    data.frame(boards, sep = "/", .)
  tmp3 <- rain_s[[i]] %>%
    tidyr::pivot_wider(names_from = ss, values_from = rain_3d_s, names_prefix = "rain.") %>%
    dplyr::select(-session) %>%
    data.frame(boards, sep = "/", .)
  tmp4 <- temp_s[[i]] %>%
    dplyr::select(-soil_s) %>%
    tidyr::pivot_wider(names_from = ss, values_from = air_s, names_prefix = "air.") %>%
    dplyr::select(-session) %>%
    data.frame(boards, sep = "/", .)
  tmp5 <- temp_s[[i]] %>%
    dplyr::select(-air_s) %>%
    tidyr::pivot_wider(names_from = ss, values_from = soil_s, names_prefix = "soil.") %>%
    dplyr::select(-session) %>%
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
  group_by(id, session, ss) %>%
  select(id, session, ss) %>%
  summarise(count = n()) %>%
  filter(count > 1)

counts

pcin_wide <- pcin_1 %>%
  # select(-Row, -flag) %>%
  filter(!is.na(id)) %>%
  # distinct() %>%
  group_by(id, session, ss) %>%
  select(id, session, ss) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(id) %>%
  tidyr::pivot_wider(names_from = c("session", "ss"), values_from = count, names_sort = TRUE, names_prefix = "")
pcin_wide

# more duplicate checking
wide_summary <- pcin_wide %>%
  ungroup() %>%
  mutate(max = apply(select(., starts_with("p")), 1, max, na.rm = TRUE),
         sum = rowSums(select(., starts_with("p")), na.rm = TRUE)) %>%
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
sess_obs_1 <- pcin_1 %>%
  group_by(session) %>%
  dplyr::select(session, ss) %>%
  summarise(K = max(ss, na.rm = TRUE)) %>%
  arrange(session)

sess_obs == sess_obs_1

# Check sex for consistency by mark (this is the problem for plot 1)

for(i in 1:length(tdf)) {
  print(dim(tdf[[i]]))
}

pp_dates <- sally %>%
  group_by(pp) %>%
  dplyr::select(pp, date) %>%
  summarise(date = median(date, na.rm = TRUE)) %>%
  arrange(pp) %>%
  as.data.frame()

session_df <- session_df %>%
  left_join(pp_dates)

saveRDS(session_df, file = here::here("analysis", "data", "derived_data", "session_df.rds"))

save(pcin_1, tdf, stds, session_df, boards, pp_obs, file = here::here("analysis", "data", "derived_data", "oSCR_prepped.Rdata"))

rm(list = ls())
gc()
