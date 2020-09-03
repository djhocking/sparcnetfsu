library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(here)
library(oSCR)
library(tidyr)

sally <- read_csv(here("analysis", "data", "raw_data", "salamander.csv"))

sally[which(sally$id == "RRRR or RRRX"), "id"] <- "RRRR"

foo <- sally %>%
  filter(plethodontid == "Y")
bar <- sally %>%
  filter(plethodontid != "Y")

foo <- foo %>%
  mutate(testes = if_else(sex1 %in% c("M") | testes1 %in% c("Y"), "Y", "N"),
         Neggs = if_else(is.na(Neggs), 0, Neggs),
         sex = "U") %>%
  mutate(cirri = if_else(snout1 %in% c("cirri", "Cirri", "CIRRI") | cirri1 == 1, "Y", "N"),
         eggs = if_else((eggs1 %in% c("Y", "1")) | (Neggs > 0), "Y", "N")) %>%
  mutate(recap = if_else(recap1 %in% c("R", "Y"), "Y", recap1)) # %>%
  # mutate(id = if_else(!is.na(id), id,
  #                     if_else(!is.na(markf), markf,
  #                             if_else(!is.na(mark2), mark2,
  #                                     if_else(!is.na(mark1), mark1, NA_character_)))))

foo[which(foo$testes == "Y" & foo$svl >= 37), "sex"] <- "M"
foo[which(foo$testes == "N" & foo$svl >= 37), "sex"] <- "F"
foo[which(foo$eggs == "Y"), "sex"] <- "F"


# find duplicates
counts <- foo %>%
  # select(-Row, -flag) %>%
  filter(!is.na(id)) %>%
  # distinct() %>%
  group_by(plot, species, sex, id, pp, ss) %>%
  select(plot, species, sex, id, pp, ss) %>%
  summarise(count = n()) %>%
  filter(count > 1)

# counts <- sally %>%
#   # select(-Row, -flag) %>%
#   # filter(!is.na(id)) %>%
#   left_join(counts) %>%
#   arrange(desc(count))
# fu <- left_join(foo, counts) %>%
  # arrange(desc(count))

# write_csv(counts, here::here("analysis", "data", "raw_data", "sally_counts.csv"))

# deal with chaning sex designations
sna <- foo %>%
  filter(!is.na(id)) %>%
  select(plot, species, sex, id, pp, ss) %>%
  arrange(species, sex, id, pp, ss) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = c(pp, ss), values_from = count, names_sort = TRUE)

dups <- foo %>%
  filter(!is.na(id)) %>%
  select(plot, species, sex2, id, pp, ss) %>%
  arrange(species, sex2, id, pp, ss) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = c(pp, ss), values_from = count, names_sort = TRUE, values_fn = length)

dups[ , "max"] <- apply(dups[ , 5:29], 1, max, na.rm = TRUE)

dups %>%
  filter(max > 1)

sal <- bind_rows(foo, bar)

write_csv(sal, here::here("analysis", "data", "raw_data", "salamander.csv"))

