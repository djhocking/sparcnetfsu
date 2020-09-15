



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


if(testing) {
  pcin_1_ss <- make.ssDF(scrFrame = pcin_1_oscr$scrFrame, buffer = 1, res = 1)
} else {
  pcin_1_ss <- make.ssDF(scrFrame = pcin_1_oscr$scrFrame, buffer = 3, res = 0.5)
}
plot(ssDF = pcin_1_ss, scrFrame = pcin_1_oscr$scrFrame)

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
                             sig ~ 1), #space use
                scrFrame = pcin_1_oscr$scrFrame,
                ssDF = pcin_1_ss)

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

# add season


# save all
save.image(here::here("analysis", "results", "mod_results.RData"))


# model comparison
fl <- fitList.oSCR(list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12),
                   drop.asu=T, rename=TRUE) #rename=T adds sesnible model names

fl <- fitList.oSCR(list(m0, m1),
                   drop.asu=T, rename=TRUE)

ms <- modSel.oSCR(fl)
ms

#pick the model with the lowest AIC
topmod <- fl[[which.min(sapply(fl,function(x) x$AIC))]]


#make a dataframe of values for DENSITY predictions
d.pred.df <- data.frame(session = rep(factor(1:length(tdf)), 2)) #session specific
#now predict
d.preds <- get.real(model = topmod, type = "dens", newdata = d.pred.df, d.factor = length(tdf))
d.preds$sex <- factor(d.preds$sex)
levels(d.preds$sex) <- c("Female","Male")
head(d.preds)

# add facet grid to separate plots and primary periods
ggplot(d.preds, aes(x=session, y=estimate, color = sex, group=sex)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=0, size=0.75, color=1,
                position = position_dodge(width=0.5)) +
  geom_point(size=5, position = position_dodge(width=0.5)) +
  theme_bw() + scale_color_fivethirtyeight() +
  xlab("Session") + ylab("Density (per m^2)")

# model m1
#make a dataframe of values for DENSITY predictions
d.pred.df <- data.frame(session = rep(factor(1:length(tdf)), 1)) #session specific
#now predict
d.preds <- get.real(model = m1, type = "dens", newdata = d.pred.df, d.factor = length(tdf))
# d.preds$sex <- factor(d.preds$sex)
# levels(d.preds$sex) <- c("Female","Male")
# head(d.preds)

# add facet grid to separate plots and primary periods
ggplot(d.preds, aes(x=session, y=estimate, color = sex, group=sex)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=0, size=0.75, color=1,
                position = position_dodge(width=0.5)) +
  geom_point(size=5, position = position_dodge(width=0.5)) +
  theme_bw() + scale_color_fivethirtyeight() +
  xlab("Session") + ylab("Density (per m^2)")

