library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

raw1 <- read_csv("Raw/raw1.csv") %>%
  mutate(Date = ymd(Date))
raw2 <- read_csv("Raw/raw2.csv") %>%
  mutate(Date = ymd(Date))
raw3 <- read_csv("Raw/raw3.csv") %>%
  mutate(Date = ymd(Date))

summary(raw1)
summary(raw2)
summary(raw3)

raw <- bind_rows(raw1, raw2, raw3) %>%
  mutate(SVL1 = as.numeric(SVL1),
         SVL2 = as.numeric(SVL2))

raw <- raw %>%
  rename(svl = SVL, tl = TL, id = ID) %>%
  mutate(MarkF = ifelse(is.na(MarkF), Mark1, MarkF),
         id = ifelse(is.na(id), MarkF, id),
         # svl = mean(SVL1, SVL2, na.rm = TRUE),
         svl = as.numeric(ifelse(is.na(svl), SVL1, svl)),
         tl = as.numeric(ifelse(is.na(tl), TL1, tl)),
         Date = ymd(Date),
         PP = NA_integer_,
         SS = NA_integer_,
         days = NA_integer_) %>%
  select(-SVL1, -SVL2, -TL1, -TL2) %>%
  ungroup() %>%
  arrange(Date, Plot, id)

summary(raw)

raw$PP[1] <- 1
raw$days[1] <- 0

for(i in 2:nrow(raw)) {
  raw$days[i] <- raw$Date[i] - raw$Date[i-1]
  raw$PP[i] <- ifelse(raw$days[i] < 30, raw$PP[i-1], raw$PP[i-1] + 1)
}

raw <- raw %>%
  ungroup() %>%
  arrange(Plot, Date, id)

raw$SS[1] <- 1
raw$days[1] <- 0

for(i in 2:nrow(raw)) {
  raw$days[i] <- raw$Date[i] - raw$Date[i-1]
  raw$SS[i] <- ifelse(raw$Plot[i] == raw$Plot[i-1] && raw$PP[i] == raw$PP[i-1] && raw$days[i] > 4, 
                      raw$SS[i-1] + 1, 
                      ifelse(raw$Plot[i] == raw$Plot[i-1] && raw$PP[i] == raw$PP[i-1],
                             raw$SS[i-1],
                             1))
}

raw$session <- NA_integer_
raw$session[1] <- 1
for(i in 2:nrow(raw)) {
  raw$session[i] <- ifelse(raw$Plot[i] == raw$Plot[i-1] && raw$PP[i] == raw$PP[i-1] && raw$SS[i] == raw$SS[i-1], raw$session[i-1], ifelse(raw$Plot[i] == raw$Plot[i-1], raw$session[i-1] + 1, 1))
}

raw %>%
  select(PP, SS) %>%
  distinct()
  
data.frame(raw[ , c("Date", "Plot", "PP", "SS", "days", "session")])
summary(raw)

write_csv(raw, "Raw/combined.csv")

comb <- read_csv("Raw/combined.csv") 

summary(comb)
str(comb)

pcin <- comb %>%
  select(PP, SS, Plot, session, Species, Sex, Recap, id) %>%
  filter(Plot == 4,
         Species == "PCIN") %>%
  mutate(count = 1)

pcin_wide <- pcin %>%
  group_by(PP, id) %>%
  select(PP, SS, id, count) %>%
  distinct()

pp1 <- pcin_wide %>%
  ungroup() %>%
  filter(PP == 1) %>%
  pivot_wider(names_from = SS, 
              values_from = count, 
              values_fill = list(count = 0),
              names_prefix = "SS_") %>%
  select(starts_with("SS")) %>%
  unite(ch, everything(), sep = "") %>%
  mutate(., ch = paste0(ch, " ", "1;")) %>%
  select(ch)

pp2 <- pcin_wide %>%
  ungroup() %>%
  filter(PP == 2) %>%
  pivot_wider(names_from = SS, 
              values_from = count, 
              values_fill = list(count = 0),
              names_prefix = "SS_") %>%
  select(starts_with("SS")) %>%
  unite(ch, everything(), sep = "") %>%
  mutate(., ch = paste0(ch, " ", "1;")) %>%
  select(ch)

# Add sex

# Add covariates

# Reduce to 3 and 4 sampling periods for examining value of additional samples
pp2_3 <- pcin_wide %>%
  ungroup() %>%
  filter(PP == 2) %>%
  pivot_wider(names_from = SS, 
              values_from = count, 
              values_fill = list(count = 0),
              names_prefix = "SS_") %>%
  select("SS_1", "SS_2", "SS_3") %>%
  unite(ch, everything(), sep = "") %>%
  filter(ch != "000") %>%
  mutate(., ch = paste0(ch, " ", "1;")) %>%
  select(ch)

pp2_4 <- pcin_wide %>%
  ungroup() %>%
  filter(PP == 2) %>%
  pivot_wider(names_from = SS, 
              values_from = count, 
              values_fill = list(count = 0),
              names_prefix = "SS_") %>%
  select("SS_1", "SS_2", "SS_3", "SS_4") %>%
  unite(ch, everything(), sep = "") %>%
  filter(ch != "0000") %>%
  mutate(., ch = paste0(ch, " ", "1;")) %>%
  select(ch)

pp3 <- pcin_wide %>%
  ungroup() %>%
  filter(PP == 3) %>%
  pivot_wider(names_from = SS, 
              values_from = count, 
              values_fill = list(count = 0),
              names_prefix = "SS_") %>%
  select(starts_with("SS")) %>%
  unite(ch, everything(), sep = "") %>%
  mutate(., ch = paste0(ch, " ", "1;")) %>%
  select(ch)

pp4 <- pcin_wide %>%
  ungroup() %>%
  filter(PP == 4) %>%
  pivot_wider(names_from = SS, 
              values_from = count, 
              values_fill = list(count = 0),
              names_prefix = "SS_") %>%
  select(starts_with("SS")) %>%
  unite(ch, everything(), sep = "") %>%
  mutate(., ch = paste0(ch, " ", "1;")) %>%
  select(ch)

pp5 <- pcin_wide %>%
  ungroup() %>%
  filter(PP == 5) %>%
  pivot_wider(names_from = SS, 
              values_from = count, 
              values_fill = list(count = 0),
              names_prefix = "SS_") %>%
  select(starts_with("SS")) %>%
  unite(ch, everything(), sep = "") %>%
  mutate(., ch = paste0(ch, " ", "1;")) %>%
  select(ch)

pp1
pp2
pp3
pp4
pp5

write.table(as.matrix(pp1), "sparcnet_pp1.inp", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(as.matrix(pp2_3), "sparcnet_pp2_3.inp", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(as.matrix(pp2_4), "sparcnet_pp2_4.inp", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(as.matrix(pp2), "sparcnet_pp2.inp", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(as.matrix(pp3), "sparcnet_pp3.inp", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(as.matrix(pp4), "sparcnet_pp4.inp", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(as.matrix(pp5), "sparcnet_pp5.inp", row.names = FALSE, quote = FALSE, col.names = FALSE)
  
# survival CJS
cjs_wide <- pcin %>%
  group_by(PP, id) %>%
  select(PP, id, count) %>%
  distinct() %>%
  pivot_wider(names_from = PP, 
              values_from = count, 
              values_fill = list(count = 0),
              names_prefix = "PP_") %>%
  mutate(ch = paste0(PP_1, PP_2, PP_3, PP_4, PP_5, " ", "1;"))

cjs <- cjs_wide %>%
  ungroup() %>%
  select(ch)

write.table(as.matrix(cjs), "sparcnet_cjs.inp", row.names = FALSE, quote = FALSE, col.names = FALSE)

# Robust Design
robust_pcin <- pcin %>%
  ungroup() %>%
  arrange(session, id) %>%
  select(session, id, count) %>%
  distinct() %>%
  pivot_wider(names_from = session, values_from = count, names_prefix = "ses_", values_fill = list(count = 0))

robust_pcin <- robust_pcin %>%
  select(-id) %>%
  unite(., col = "ch", starts_with("ses_"), sep = "") %>%
  group_by(ch) %>%
  summarise(count = n()) %>%
  mutate(ch = paste0(ch, " ", count, ";")) %>%
  select(ch)

sessions_per_pp <- pcin %>%
  select(PP, SS) %>%
  distinct() %>%
  group_by(PP) %>%
  summarise(SS = max(SS))

write.table(as.matrix(robust_pcin), "sparcnet_robust.inp", row.names = FALSE, quote = FALSE, col.names = FALSE)

write.csv(sessions_per_pp, "sparcnet_sessions.csv", row.names = FALSE, quote = FALSE)
