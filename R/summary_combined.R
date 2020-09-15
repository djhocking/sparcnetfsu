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

fl <- fitList.oSCR(list(m0, m1, m3),
                   drop.asu=T, rename=TRUE)

ms <- modSel.oSCR(fl)
ms

#pick the model with the lowest AIC
topmod <- fl[[which.min(sapply(fl,function(x) x$AIC))]]


#make a dataframe of values for DENSITY predictions
d.pred.df <- data.frame(session = rep(factor(1:length(tdf)), 1)) #session specific
#now predict
d.preds <- get.real(model = topmod, type = "dens", newdata = d.pred.df, d.factor = length(tdf))
d.preds$sex <- factor(d.preds$sex)
levels(d.preds$sex) <- c("Female","Male")
head(d.preds)

session_df <- session_df %>%
  dplyr::mutate(Session = session,
                session = as.integer(as.factor(Session)))

d.preds <- d.preds %>%
  dplyr::mutate(session = as.integer(as.factor(session))) %>%
  left_join(session_df)

# add facet grid to separate plots and primary periods
ggplot(d.preds, aes(x=pp, y=estimate, color = sex, group=sex)) +
  geom_line(size = 1, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=0, size=0.75, color=1,
                position = position_dodge(width=0.5)) +
  geom_point(size=4, position = position_dodge(width=0.5)) +
  facet_wrap(~plot) +
  theme_bw() + # scale_color_fivethirtyeight() +
  xlab("Session") + ylab(expression(Density~(per~m^2)))

ggsave("analysis/figures/density_session.pdf")


ggplot(d.preds, aes(x=date, y=estimate, color = sex, group=sex)) +
  geom_line(size = 1, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=0, size=0.75, color=1,
                position = position_dodge(width=0.5)) +
  geom_point(size=4, position = position_dodge(width=0.5)) +
  facet_wrap(~plot) +
  theme_bw() + # scale_color_fivethirtyeight() +
  xlab("Date") + ylab(expression(Density~(per~m^2))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("analysis/figures/density_date.pdf")
