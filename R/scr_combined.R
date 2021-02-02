



#---------- Setup ----------
# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(here)
library(tidyr)
library(oSCR)

# data
load(here::here("analysis", "data", "derived_data", "oSCR_prepped.Rdata"))

# testing
testing <- FALSE

if(!dir.exists("analysis/results")) dir.create("analysis/results")

#-------- Set up oSCR data ---------



pcin_1_oscr <- data2oscr(edf = pcin_1,
                         sess.col = 1,
                         id.col = 2,
                         occ.col = 3,
                         trap.col = 4,
                         sex.col = 5,
                         tdf = tdf,
                         K = pp_obs$K,
                         ntraps = rep(50, length(pp_obs$K)),
                         trapcov.names = c("doys", "doys2", "rain", "air", "soil"),
                         tdf.sep = "/",
                         sex.nacode = "U")

names(pcin_1_oscr)
names(pcin_1_oscr$scrFrame)
pcin_1_oscr$scrFrame

par(mfrow = c(6, 6), mar = c(2, 2, 2, 2), oma = c(0, 0, 0, 0))
plot(pcin_1_oscr$scrFrame, jit = 2)

par(mfrow = c(2, 2), mar = c(2, 2, 2, 2), oma = c(0, 0, 0, 0))
pdf("analysis/figures/spatial_cap_example.pdf")
plot(pcin_1_oscr$scrFrame, jit = 2)
dev.off()
par(mfrow = c(1, 1))

par(mfrow = c(2, 2), mar = c(2, 2, 2, 2), oma = c(0, 0, 0, 0))
png("analysis/figures/spatial_cap_example.png")
plot(pcin_1_oscr$scrFrame, jit = 2)
dev.off()
par(mfrow = c(1, 1))


if(testing) {
  pcin_1_ss <- make.ssDF(scrFrame = pcin_1_oscr$scrFrame, buffer = 1, res = 1)
} else {
  pcin_1_ss <- make.ssDF(scrFrame = pcin_1_oscr$scrFrame, buffer = 3, res = 0.5)
}
pdf("analysis/figures/state_space_grid.pdf")
plot(ssDF = pcin_1_ss, scrFrame = pcin_1_oscr$scrFrame)
dev.off()

png("analysis/figures/state_space_grid.png")
plot(ssDF = pcin_1_ss, scrFrame = pcin_1_oscr$scrFrame)
dev.off()

#---------- SCR Analysis ---------
#
# m16 <- oSCR.fit(model = list(D ~ session, #density
#                              p0 ~ b + session + sex, #detection - fails with behavioral model for just one plot
#                              sig ~ 1 + sex), #space use
#                 scrFrame = pcin_1_oscr$scrFrame, ssDF = pcin_1_ss)

m0 <- oSCR.fit(model = list(D ~ 1, #density
                            p0 ~ 1, #detection
                            sig ~ 1), #space use
               scrFrame = pcin_1_oscr$scrFrame, ssDF = pcin_1_ss)

m0b <- oSCR.fit(model = list(D ~ 1, #density
                            p0 ~ 1, #detection
                            sig ~ sex), #space use
               scrFrame = pcin_1_oscr$scrFrame, ssDF = pcin_1_ss)


m1 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ air, #detection
                             sig ~ 1), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)

m2 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ soil, #detection
                             sig ~ 1), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)

m3 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ soil + I(soil^2), #detection
                             sig ~ 1), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)

m4 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ rain, #detection
                             sig ~ 1), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)

m5 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ soil * rain, #detection
                             sig ~ 1), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)

m6 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ soil + I(soil^2) + soil * rain, #detection
                             sig ~ 1), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)

m7 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ b, #detection
                             sig ~ 1), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)

m8 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ b + soil*rain, #detection
                             sig ~ sex), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)

saveRDS(m8, file = here::here("analysis", "results", "m8.rds"))

m8b <- oSCR.fit(model = list(D ~ session, #density
                            p0 ~ b + soil*rain, #detection
                            sig ~ 1), #space use
               scrFrame = pcin_1_oscr$scrFrame,
               ssDF = pcin_1_ss)

saveRDS(m8b, file = here::here("analysis", "results", "m8b.rds"))

m9 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ b + soil*rain, #detection
                             sig ~ session), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)

m10 <- oSCR.fit(model = list(D ~ session, #density
                             p0 ~ sex, #detection - fails with behavioral model for just one plot
                             sig ~ 1 + sex), #space use
                scrFrame = pcin_1_oscr$scrFrame, ssDF = pcin_1_ss)

m11 <- oSCR.fit(model = list(D ~ session + sex, #density
                             p0 ~ b + sex + soil*rain, #detection
                             sig ~ session + sex), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)

m12 <- oSCR.fit(model = list(D ~ session + sex, #density
                             p0 ~ sex + sex*soil + sex*rain, #detection
                             sig ~ session + sex), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)


m13 <- oSCR.fit(model = list(D ~ session + sex, #density
                            p0 ~ b + soil + I(soil^2) + rain + soil * rain, #detection
                            sig ~ session + sex), #space use
               scrFrame = pcin_1_oscr$scrFrame,
               ssDF = pcin_1_ss)

# add season

# save all
save.image(here::here("analysis", "results", "mod_results.RData"))

# cleanup
rm(list = ls())
gc()


