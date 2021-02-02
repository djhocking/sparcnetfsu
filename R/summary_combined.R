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

topmod <- m8

#make a dataframe of values for DENSITY predictions
d.pred.df <- data.frame(session = rep(factor(1:length(tdf)), 1)) #session specific
#now predict
d_preds <- get.real(model = topmod, type = "dens", newdata = d.pred.df, d.factor = 4)
d_preds$sex <- factor(d_preds$sex)
levels(d_preds$sex) <- c("Female","Male")
head(d_preds)

session_df <- session_df %>%
  dplyr::mutate(Session = session,
                session = as.integer(as.factor(Session)))

d_preds <- d_preds %>%
  dplyr::mutate(session = as.integer(as.factor(session))) %>%
  left_join(session_df)

# add facet grid to separate plots and primary periods
ggplot(d_preds, aes(x=pp, y=estimate, color = sex, group=sex)) +
  geom_line(size = 1, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=0, size=0.75, color=1,
                position = position_dodge(width=0.5)) +
  geom_point(size=4, position = position_dodge(width=0.5)) +
  facet_wrap(~plot) +
  theme_bw() + # scale_color_fivethirtyeight() +
  xlab("Session") + ylab(expression(Density~(per~m^2)))

ggsave("analysis/figures/density_session.pdf")


ggplot(d_preds, aes(x=date, y=estimate, color = sex, group=sex)) +
  geom_line(size = 1, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=0, size=0.75, color=1,
                position = position_dodge(width=0.5)) +
  geom_point(size=4, position = position_dodge(width=0.5)) +
  facet_wrap(~plot) +
  theme_bw() + # scale_color_fivethirtyeight() +
  xlab("Date") + ylab(expression(Density~(per~m^2))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("analysis/figures/density_date.pdf")

# Spatial and Temporal Variance/SD (in the means)

# variation among plots
plot_means <- d_preds %>%
  group_by(plot) %>%
  summarise(mean = mean(estimate, na.rm = TRUE),
            sd = sd(estimate, na.rm = TRUE))

spatial_sd <- sd(plot_means$mean)

# variation among sessions (primary periods)
session_means <- d_preds %>%
  group_by(session) %>%
  summarise(mean = mean(estimate, na.rm = TRUE),
            sd = sd(estimate, na.rm = TRUE))

temporal_sd <- sd(session_means$mean) # variation over time (averaged over space)

# Sex ratios
sex_samps <- sapply(pcin_1_oscr$scrFrame$indCovs, function(x) table(x$sex))
sex_samps <- rbind(sex_samps, round(sex_samps[1,] / apply(sex_samps,2,sum),2))
dimnames(sex_samps) <- list(c("F","M","Prop.F"), paste0("S",1:36))
sex_samps

s_pred_df <- data.frame(session = rep(factor(1:36),2),
                        sex = rep(c(0,1),each=36))
#now predict
s_preds <- get.real(model = topmod, type = "sig", newdata = s_pred_df)
s_preds$sex <- factor(s_preds$sex)
levels(s_preds$sex) <- c("Female","Male")
head(s_preds)

s_pred_df <- data.frame(session = rep(factor(1:36),2),
                        sex = rep(c(0,1),each=36))
s_preds <- get.real(model = m0, type = "sig", newdata = s_pred_df)
s_preds$sex <- factor(s_preds$sex)
levels(s_preds$sex) <- c("Female","Male")
head(s_preds)

s_pred_df <- data.frame(sex = rep(c(0,1),each=1), sig.sex = rep(c(0,1),each=1),
                        sexmale = rep(c(0,1),each=1))
s_preds <- get.real(model = m0b, type = "sig", newdata = s_pred_df)
  s_preds$sex <- factor(s_preds$sex)
levels(s_preds$sex) <- c("Female","Male")
head(s_preds)

# calculate 90% Home Range - delta method for uncertainty
s_preds <- s_preds %>%
  dplyr::mutate(session = as.integer(as.factor(session)),
                hr90_estimate = pi() * (estimate * 1.282) ^ 2,
                hr90_sd = sqrt(sd^2 * (2 * pi() * estimate)^2),
                hr90_lwd = estimate - 1.96 * hr90_sd,
                hr90_upr = estimate + 1.96 * hr90_sd) %>%
  left_join(session_df)


ggplot(s_preds, aes(x=pp, y=estimate, color = sex, group=sex)) +
  geom_line(size = 1, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=lwr,ymax=upr), width=0, size=0.75, color=1,
                position = position_dodge(width=0.5)) +
  geom_point(size=4, position = position_dodge(width=0.5)) +
  facet_wrap(~plot) +
  theme_bw() + # scale_color_fivethirtyeight() +
  xlab("Session") + ylab(expression(Sigma~(m^2)))

ggsave("analysis/figures/sigma_session.pdf")

ggplot(s_preds, aes(x=pp, y=hr90_estimate, color = sex, group=sex)) +
  geom_line(size = 1, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=hr90_lwr,ymax=hr90_upr), width=0, size=0.75, color=1,
                position = position_dodge(width=0.5)) +
  geom_point(size=4, position = position_dodge(width=0.5)) +
  facet_wrap(~plot) +
  theme_bw() + # scale_color_fivethirtyeight() +
  xlab("Session") + ylab(expression(Sigma~(m^2)))

ggsave("analysis/figures/home_range_session.pdf")

