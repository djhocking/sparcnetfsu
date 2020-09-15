# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(here)
library(tidyr)
library(oSCR)

# load all
load(here::here("analysis", "results", "mod_results.RData"))

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

